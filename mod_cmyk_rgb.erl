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

    SUB_DIR = "tmp",
    
    %% Move temporary file to processing directory
    Dir = z_path:files_subdir_ensure("archive/" ++ SUB_DIR, Context),
    Target = filename:join([Dir, OriginalFilename]),
    file:delete(Target),
    {ok, _} = file:copy(TmpFile, Target),
    file:delete(TmpFile),
    
    Feedback = case TmpFile of
        [] ->
            {error, "No file"};
        _ ->
            Cmd = "identify -verbose " ++ z_utils:os_filename(Target),
            ImageInfo = os:cmd(Cmd),
            Format = parseImageMagickInfo(ImageInfo, "Format"),
            FormatKey = lists:nth(1, string:tokens(Format, " ")),
            handleImageFormat(FormatKey, ImageInfo, OriginalFilename, SUB_DIR, Target, Context)            
        end,
    
    file:delete(Target),
    
    AlertClass = case Feedback of 
        {ok, _, _} -> "alert alert-success";
        {info, _, _} -> "alert alert-info";
        {error, _, _} -> "alert alert-danger"
    end,
    {_, FeedbackMessage, File} = Feedback,
    Html = "<div class='" ++ AlertClass ++ "'>" ++ FeedbackMessage ++ "</div>",
    Context2 = z_render:update_selector(".mod-cmyk-rgb-upload-feedback", Html, Context),
    Context3 = case File of
        undefined ->
            Context2;
        File -> 
            lager:warning("download file ~p", [File]),
            z_render:wire({redirect, [{dispatch, "media_attachment"}, {star, SUB_DIR ++ "/" ++ File}]}, Context2)
    end,
    
%    Archive = z_path:media_archive(Context3),
%    lager:warning("delete:~p", [Archive ++ File]),
%    file:delete(Archive ++ "/" ++ File),
    Context3.


parseImageMagickInfo(Info, Key) ->
    Lines = string:tokens(Info, "\r\n"),
    Values = lists:map(fun(Line) ->
        Parts = re:split(string:strip(Line), "\s?:\s?", [{return, list}]),
        case Parts of 
            [Key, Value] -> Value;
            _ -> undefined
        end
    end, Lines),
    first(Values, fun(E) when E =:= undefined -> false;
        (_) -> true
    end, undefined).

first([E | Rest], Condition, Default) ->
    case Condition(E) of
        true -> E;
        false -> first(Rest, Condition, Default)
    end;
first([], _Cond, Default) -> Default.


handleImageFormat(Format, ImageInfo, OriginalFilename, SubDir, Target, Context) when Format =:= "JPEG"; Format =:= "PNG"; Format =:= "TIFF" -> 
    ColorSpace = parseImageMagickInfo(ImageInfo, "Colorspace"),
    lager:warning("ColorSpace: ~p", [ColorSpace]),
    case ColorSpace of
        "RGB" ->
            {ok, "Nothing to do: file is already RGB.", undefined};
        "sRGB" ->
            {ok, "Nothing to do: file is already RGB.", undefined};
        "CMYK" ->
            convertImage(OriginalFilename, SubDir, Target, Context);
        _ ->
            {info, "Unknown colorspace, keep as is.", undefined}
    end;
handleImageFormat(_, _, _, _, _, _) ->
    {error, "Unsupported file format.", undefined}.
            
            
%% convertImage(OriginalFilename, SubDir, InputFile, Context) -> {ok, Info:String, File:String} | {error, Reason:String, undefined}
convertImage(OriginalFilename, SubDir, InputFile, Context) ->
    OutputFile = OriginalFilename ++ "-rgb" ++ "." ++ "png",
    Dir = z_path:files_subdir_ensure("archive/" ++ SubDir, Context),
    OutputPath = filename:join([Dir, OutputFile]),
    ConvertCmd = lists:flatten([
        "convert ",
        InputFile, " ",
        "-colorspace sRGB ",
        OutputPath
    ]),
    lager:warning("ConvertCmd=~p", [ConvertCmd]),
    % execute imagemagick command
    z_media_preview_server:exec(ConvertCmd, OutputPath),
    case filelib:is_regular(OutputPath) of
        true ->
            {ok, "Converted CMYK to RGB.", OutputFile};
        false ->
            {error, "Could not convert image to RGB.", undefined}
    end.



