define(["require", "jquery", "underscore", "monster"], function(e) {
    var $ = e("jquery"),
        _ = e("underscore"),
        monster = e("monster"),
        app = {
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
            load: function(callback) {
                var self = this;
                self.initApp(function() {
                    callback && callback(self)
                })
            },
            initApp: function(callback) {
                var self = this;
                monster.pub("auth.initApp", {
                    app: self,
                    callback: callback
                })
            },
            render: function(callback) {
                var self = this,
                    content = callback || $("#monster-content"),
                    template = $(monster.template(self, "app"));
                    // accountid switch urs
                    _.accountId = monster.apps.auth.accountId;
                template.find(".category#phonebook").addClass("active"), monster.pub("phonebook.phonebook.render", {
                    parent: template.find(".right-content")
                }), self.bindEvents(template), content.empty().append(template)
            },
            bindEvents: function(callback) {
                var self = this,
                    content = callback.find(".right-content");
                callback.find(".category").on("click", function() {
                    var self = $(this),
                        option = {
                            parent: content
                        },
                        attribut = self.attr("id");
                    callback.find(".category").removeClass("active"), self.toggleClass("active"), content.empty(), monster.pub("phonebook." + attribut + ".render", option)
                })
            }
        };
    return app
});