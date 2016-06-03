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
                        row = data.context.dataset.row,
                        s = self.appFlags.tableData[row][5],
                        detail = $(monster.template(self, "phonebook-detail", {
                            metadata: s
                        }));
                    detail.find("#close").on("click", function() {
                        edit.dialog("close").remove()
                    }), detail.find(".book-details-save").on("click", function() {

                        detail.find(".book-data, .book-details-hide").show(), detail.find(".book-details").hide()
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
                        fnRender: function(table) {
//console.log(table);
                            return '<a href="#" class="detail-link monster-link blue" data-row="' + table.iDataRow + '" id="'+ table.aData[] +'"><i class="fa fa-eye"></i></a>'
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
                        self.appFlags.tableData = ret, callback && callback(ret)
                    }
                })
            }
        };
    return app
});