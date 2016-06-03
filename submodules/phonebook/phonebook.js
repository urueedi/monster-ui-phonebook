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
            phonebookRender: function(e) {
                var self = this,
                    e = e || {},
                    i = e.parent || $(".right-content"),
                    template = $(monster.template(self, "phonebook-layout"));
                self.phonebookInitTable(template, function() {
                    self.phonebookBindEvents(template), i.empty().append(template)
                })
            },
            phonebookBindEvents: function(container) {
                var self = this;
                container.on("click", ".detail-link", function() {
                    var data = $(this),
                        row = container.data("row"),
                        s = container.appFlags.tableData[row][5],
                        o = $(monster.template(container, "phonebook-detail", {
                            metadata: s
                        }));
console.log(row);
                    o.find("#close").on("click", function() {
                        u.dialog("close").remove()
                    }), o.find(".book-details").on("click", function() {
                        o.find(".book-data, .book-details-hide").show(), o.find(".book-details").hide()
                    }), o.find(".book-details-hide").on("click", function() {
                        o.find(".book-details").show(), o.find(".book-data, .book-details-hide").hide()
                    });
                    var u = monster.ui.dialog(o, {
                        title: self.i18n.active().phonebook.detailDialog.popupTitle,
                        position: ["center", 20]
                    })
                })
            },
            phonebookInitTable: function(template, func) {
                var self = this,
                    table = [{
                        sTitle: self.i18n.active().phonebook.tableTitles.country
                    }, {
                        sTitle: self.i18n.active().phonebook.tableTitles.company
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
                        fnRender: function(data) {
                            return '<a href="#" class="detail-link monster-link blue" data-row="' + data.iDataRow + '"><i class="fa fa-eye"></i></a>'
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
                    ret.push([this.country, this.company, this.name, this.address, this.zip, this.city, this.id])
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
console.log(ret);
                        self.appFlags.tableData = ret, callback && callback(ret)
                    }
                })
            }
        };
    return app
});