define(["require", "jquery", "underscore", "monster", "toastr"], function(require) {
    var $ = require("jquery"),
        _ = require("underscore"),
        monster = require("monster"),
        toastr = require("toastr"),
        app = {
            requests: {},
            subscribe: {
                "phonebook.phonebook.render": "phonebookRender"
            },
            appFlags: {
                tableData: []
            },
            phonebookRender: function(ext) {
                var self = this,
                    ext = ext || {},
                    inx = ext.parent || $(".right-content"),
                    template = $(monster.template(self, "phonebook-layout"));
                self.phonebookInitTable(template, function() {
                    self.phonebookBindEvents(template), inx.empty().append(template)
                })
            },
            phonebookBindEvents: function(container) {
                var self = this;
                container.on("click", ".detail-link", function() {
                    var data = $(this),
                        row = data.context.dataset.row,
                        sData = self.appFlags.tableData[row][5],
                        detail = $(monster.template(self, "phonebook-detail", {
                            metadata: sData
                        }));
                    detail.find("#cancel").on("click", function() {
                        edit.dialog("close").remove()
                    }), detail.find("#book-detail-delete").on("click", function() {
                        edit.dialog("close").remove()
                    }), detail.find("#book-detail-save").on("click", function() {
                        detail.find(".book-data, .book-details-hide").show(), detail.find(".book-details").hide(),
                        edit.dialog("close").remove()
                    });
                    var edit = monster.ui.dialog(detail, {
                        title: self.i18n.active().phonebook.detailDialog.popupTitle,
                        position: ["center", 20]
                    })
                })
            },
            phonebookInitTable: function(template, func) {
                var self = this,
                    table = [{
                        sTitle: self.i18n.active().phonebook.tableTitles.country,
                        fnRender: function(table) {
                            return '<image class="small flag" image="' + table.country + '"></image>'
                        }
                    }, {
                        sTitle: self.i18n.active().phonebook.tableTitles.name
                    }, {
                        sTitle: self.i18n.active().phonebook.tableTitles.address
                    }, {
                        sTitle: self.i18n.active().phonebook.tableTitles.zip
                    }, {
                        sTitle: self.i18n.active().phonebook.tableTitles.city
                    }, {
                        sTitle: self.i18n.active().phonebook.tableTitles.details,
                        fnRender: function(table) {
                            return '<a href="#" class="detail-link monster-link blue" data-row="' + table.iDataRow +'"><i class="fa fa-eye"></i></a>'
                        }
                    }, {
                        bVisible: !1
                    }];
                self.phonebookGetData(function(self) {
                    monster.ui.table.create("phonebook", template.find("#phonebook_grid"), table, self, {
                        sDom: '<"table-custom-actions">frtlip',
                        aaSorting: [
                            [2, "asc"]
                        ]
                    }),$.fn.dataTableExt.afnFiltering.pop(), func && func()
                })
            },
            phonebookFormatDataTable: function(data) {
                var ret = [];
                return $.each(data, function() {
                    ret.push([this.country||'', this.name||'', this.address||'', this.zip||'', this.city||'', this||''])
                }), ret
            },
            phonebookGetData: function(callback) {
                var self = this;
                self.callApi({
                    resource: "phonebook.list",
                    data: {
                        accountId: self.accountId
                    },
                    success: function(data) {
                        var ret = self.phonebookFormatDataTable(data.data);
                        self.appFlags.tableData = ret, callback && callback(ret)
                    }
                })
            }
        };
    return app
});