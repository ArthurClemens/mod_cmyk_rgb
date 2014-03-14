%% @author Arthur Clemens
%% @copyright 2014 Arthur Clemens
%% Generated on 2014-03-13
%% @doc CMYK to RGB converter

-module(mod_cmyk_rgb).
-author("Arthur Clemens").

-mod_title("CMYK to RGB converter").
-mod_description("CMYK to RGB converter.").
-mod_prio(100).

-include_lib("zotonic.hrl").

-export([
    event/2
]).

%% Submitted form
event(#submit{message={upload, []}}, Context) ->
    #upload{filename=OriginalFilename, tmpfile=TmpFile} = z_context:get_q_validated("file", Context),
    lager:warning("upload called, TmpFile=~p", [TmpFile]),

    SUB_DIR = "tmp",
    
    Feedback = case TmpFile of
        [] ->
            {error, "No file"};
        _ ->
            Cmd = "identify " ++ z_utils:os_filename(TmpFile),
            lager:warning("Cmd=~p", [Cmd]),
            ImageInfo = os:cmd(Cmd),
            lager:warning("ImageInfo: ~p", [ImageInfo]),
            Info = string:tokens(ImageInfo, " "),
            lager:warning("Info: ~p", [Info]),
            
            case length(Info) of
                9 -> 
                    [_, _, _, _, _, ColorSpace, _, _, _] = Info,
                    case ColorSpace of
                        "RGB" ->
                            {ok, "File is RGB", undefined};
                        "sRGB" ->
                            {ok, "File is RGB", undefined};
                        "CMYK" ->
                            RGBFile = OriginalFilename ++ "-rgb" ++ "." ++ "png",
                            ConvertCmd = lists:flatten([
                                "convert ",
                                TmpFile, " ",
                                "-colorspace sRGB ",
                                RGBFile
                            ]),
                            lager:warning("ConvertCmd=~p", [ConvertCmd]),
                            % execute imagemagick command
                            z_media_preview_server:exec(ConvertCmd, RGBFile),
                            case filelib:is_regular(RGBFile) of
                                true ->
                                    %% Move temporary file to processing directory
                                    Dir = z_path:files_subdir_ensure("archive/" ++ SUB_DIR, Context),
                                    Target = filename:join([Dir, RGBFile]),
                                    file:delete(Target),
                                    {ok, _} = file:copy(RGBFile, Target),
                                    file:delete(RGBFile),
                                    {ok, "Converted CMYK to RGB", RGBFile};
                                false ->
                                    {error, "Could not convert image to RGB", undefined}
                            end;
                        _ ->
                            {info, "Unknown colorspace, keep as is.", undefined}
                    end;
                _ ->
                    {error, "Error reading file. May not be an image file.", undefined}
            end
        end,
        
    file:delete(TmpFile),
    
    AlertClass = case Feedback of 
        {ok, _, _} -> "alert alert-success";
        {info, _, _} -> "alert alert-info";
        {error, _, _} -> "alert alert-danger"
    end,
    {_, FeedbackMessage, File} = Feedback,
    Html = "<div class='" ++ AlertClass ++ "'>" ++ FeedbackMessage ++ "</div>",
    Context2 = z_render:update_selector(".mod-cmyk-rgb-upload-feedback", Html, Context),
    Context3 = case File of
        undefined -> Context2;
        File -> 
            lager:warning("download file ~p", [File]),
            z_render:wire({redirect, [{dispatch, "media_attachment"}, {star, SUB_DIR ++ "/" ++ File}]}, Context2)
    end,
    
%    Archive = z_path:media_archive(Context3),
%    lager:warning("delete:~p", [Archive ++ File]),
%    file:delete(Archive ++ "/" ++ File),
    Context3.


