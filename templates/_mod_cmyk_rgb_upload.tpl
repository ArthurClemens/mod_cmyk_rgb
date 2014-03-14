<h3>Convert CMYK image to RGB</h3>
{% wire id=#send type="submit" postback={upload} delegate=`mod_cmyk_rgb` %}
<form class="form" id="{{ #send }}" method="post" action="postback" enctype="multipart/form-data">
    <div class="mod-cmyk-rgb-upload-feedback"></div>
    <div class="form-group">
        <input type="file" id="{{ #file }}" name="file" title="{_ Choose image file _}" />
        {% validate id=#file name="file" type={presence} %}
    </div>
    <div class="form-group">
        <input type="submit" class="btn" value="{_ Upload _}" />
    </div>
</form>