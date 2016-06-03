define(["require", "jquery", "underscore", "monster"], function(e) {
    var t = e("jquery"),
        n = e("underscore"),
        r = e("monster"),
        i = {
            name: "phonebook",
            css: ["app"],
            i18n: {
                "en-US": {customCss: !1},
                "fr-FR": {customCss: !1},
                "ro-RO": {customCss: !1},
                "nl-NL": {customCss: !1},
                "it-IT": {customCss: !1},
                "de-DE": {customCss: !1},
                "dk-DK": {customCss: !1},
                "es-ES": {customCss: !1},
                "pt-PT": {customCss: !1},
                "ru-RU": {customCss: !1},
                "zh-CN": {customCss: !1}
            },
            requests: {},
            subscribe: {},
            subModules: ["phonebook", "speeddial", "blacklist"],
            load: function(e) {
                var t = this;
                t.initApp(function() {
                    e && e(t)
                })
            },
            initApp: function(e) {
                var t = this;
                r.pub("auth.initApp", {
                    app: t,
                    callback: e
                })
            },
            render: function(e) {
                var n = this,
                    i = e || t("#monster-content"),
                    s = t(r.template(n, "app"));
                    // accountid switch urs
                    n.accountId = r.apps.auth.accountId;
                s.find(".category#phonebook").addClass("active"), r.pub("phonebook.phonebook.render", {
                    parent: s.find(".right-content")
                }), n.bindEvents(s), i.empty().append(s)
            },
            bindEvents: function(e) {
                var n = this,
                    i = e.find(".right-content");
                e.find(".category").on("click", function() {
                    var n = t(this),
                        s = {
                            parent: i
                        },
                        o = n.attr("id");
                    e.find(".category").removeClass("active"), n.toggleClass("active"), i.empty(), r.pub("phonebook." + o + ".render", s)
                })
            }
        };
    return i
});