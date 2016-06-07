define(["require", "jquery", "underscore", "monster", "toastr", "monster-flags", "chosenImage"], function(require) {
    var $ = require("jquery"),
        _ = require("underscore"),
        monster = require("monster"),
        toastr = require("toastr"),
        quickcalldevice = require('monster-quickcalldevice'),
        flags = require('monster-flags'),
        chosenImage = require('chosenImage'),

        app = {
            requests: {},
            subscribe: {
                "phonebook.phonebook.render": "phonebookRender"
            },
            appFlags: {
                tableData: []
            },
            phonebookRender: function(content) {
                var self = this,
                    content = content || {},
                    index = content.parent || $(".right-content"),
                    template = $(monster.template(self, "phonebook-layout"));
                self.phonebookInitTable(template, function() {
                    self.phonebookBindEvents(template), index.empty().append(template)
                })
            },
            phonebookBindEvents: function(container) {
                var self = this;

                // upload phonebook
                container.on("click", "#upload-link", function() {
                    var data = $(this),
                        detail = $(monster.template(self, "upload", {
                            metadata: data
                        }));
                    detail.find(".cancel-link").on("click", function() {
                        edit.dialog("close").remove()

                    });
                    var edit = monster.ui.dialog(detail, {
                        title: self.i18n.active().phonebook.uploadDialog.popupTitle,
                        position: ["center", 20]
                    });
                }),
                // delete phoneentry
                container.on("click", "#delete-phonebook-link", function() {
                    var data = $(this);
                        var checkedValues = $("input:checkbox:checked", "#phonebook_grid").map(function() {
                            return $(this).val();
                        }).get(); delete checkedValues['on'];
                        $.each(checkedValues, function(i, id) {
                                var entry = {};
                                entry.id = encodeURIComponent(id);
                                self.phonebookDelete(entry, function(data) {
                                });
                        });
                        if(checkedValues.length > 0) {
                            self.render();
                            toastr.success(monster.template(self, '!' + self.i18n.active().phonebook.deleteSuccess ))
                        }
                }),
                // add phoneentry
                container.on("click", "#add-phonebook-link", function() {
                    var data = $(this),
                        detail = $(monster.template(self, "phonebook-add", {
                            metadata: ''
                        }));
                    detail.find("#cancel").on("click", function() {
                        edit.dialog("close").remove()

                    }), detail.find("#book-detail-add").on("click", function() {
                        var formData = monster.ui.getFormData('phonebook_detail_dialog');
//                        formData.metadata.id = encodeURIComponent(formData.metadata.id);
                        self.phonebookAdd(formData.metadata, function(data) {
                                toastr.success(monster.template(self, '!' + self.i18n.active().phonebook.addSuccess + data.id ));
                                self.render();
                                edit.dialog('close').remove();
                        });
                    });
                    flags.populateDropdown(detail.find('#metadata_country'), 'inherit', {inherit: ''});
                    detail.find('#metadata_country').chosenImage({ search_contains: true, width: '220px' });
                    var edit = monster.ui.dialog(detail, {
                        title: self.i18n.active().phonebook.detailDialog.popupTitle,
                        position: ["center", 20]
                    });
                }),
                // edit phoneentry
                container.on("click", ".detail-link", function() {
                    var data = $(this),
                        row = data.context.dataset.row,
                        sData = self.appFlags.tableData[row][6],
                        detail = $(monster.template(self, "phonebook-edit", {
                            metadata: sData
                        }));
                    detail.find("#cancel").on("click", function() {
                        edit.dialog("close").remove()

                    }), detail.find("#book-detail-delete").on("click", function() {
                        var formData = monster.ui.getFormData('phonebook_detail_dialog');
                        formData.metadata.id = encodeURIComponent(formData.metadata.id);
                        self.phonebookDelete(formData.metadata, function(data) {
                                self.render();
                                edit.dialog('close').remove();
                                toastr.success(monster.template(self, '!' + self.i18n.active().phonebook.deleteSuccess + data.id ));
                        });

                    }), detail.find("#book-detail-update").on("click", function() {
                        var formData = monster.ui.getFormData('phonebook_detail_dialog');
                        formData.metadata.id = encodeURIComponent(formData.metadata.id);
                        self.phonebookUpdate(formData.metadata, function(data) {
                                self.render();
                                edit.dialog('close').remove();
                                toastr.success(monster.template(self, '!' + self.i18n.active().phonebook.addSuccess + data.id ));
                        });
                    });
                    flags.populateDropdown(detail.find('#metadata_country'), self.appFlags.tableData[row][1]||'inherit', {inherit: ''});
                    detail.find('#metadata_country').chosenImage({ search_contains: true, width: '220px' });
                    var edit = monster.ui.dialog(detail, {
                        title: self.i18n.active().phonebook.detailDialog.popupTitle,
                        position: ["center", 20]
                    });
                }),
                // quickcall
                container.on("click", ".link-quickcall", function() {
                    var data = $(this),
                        number = data.context.dataset.row;
                        cnumber = encodeURIComponent(number);
                        var deviceId = $("#device_quickcall", container).val();
                        deviceId && deviceId.length === 32 ? self.callApi({
                            resource: "device.quickcall",
                            data: {
                                accountId: monster.apps.auth.originalAccount.id,
                                deviceId: deviceId,
                                number: cnumber
                            },
                            success: function(template) {var startcall =  new Date().getTime(); toastr.info(self.i18n.active().phonebook.quickcall_startedto + number, '', {"timeOut": 35000});
                                var stopcall =  new Date().getTime();
                                var difftime = (stopcall*1) - (startcall*1);
//                                if(difftime < 35000)
//                                        toastr.error(self.i18n.active().phonebook.quickcall_startedto + number, '', {"timeOut": 3000})
                            }
                        }) : toastr.error(self.i18n.active().phonebook.you_need_to_select_a_registered_device, '', {"timeOut": 3000})
                });
            },
            phonebookInitTable: function(template, func) {
                var self = this,
                    table = [
                    {
                        sTitle: '<input type="checkbox" id="select_all_bookentrys"/>',
                        sWidth: "40px",
                        bSortable: !1,
                        fnRender: function(data) {
                            return '<input type="checkbox" class="select-checkbox" value="' + data.aData[7] + '"/>'
                        }
                    }, {
                        sTitle: self.i18n.active().phonebook.tableTitles.country,
                        fnRender: function(data) {
                            return '<img src="css/assets/flags/24/' + data.aData[1] + '.png">'
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
                        bSortable: false,
                        fnRender: function(data) {
                            return '<a href="#" class="detail-link monster-link blue" data-row="' + data.iDataRow +'"><i class="fa fa-edit"></i></a>'+
                                    '<a href="#" class="link-quickcall user_quickcalldevice monster-link blue" data-row="' + data.aData[7] +'"><i class="fa fa-phone"></i></a>'
                        }
                    }, {
                        bVisible: !1
                    }];
                self.phonebookGetData(function(data) {
                    monster.ui.table.create("phonebook", template.find("#phonebook_grid"), table, data, {
                        sDom: '<"actions_quickcalldevice"><"actions_phonebook">frtlip',
                        aaSorting: [
                            [3, "asc"]
                        ]
                }),
                $.fn.dataTableExt.afnFiltering.pop(),func && func(),

                $("div.actions_quickcalldevice", template).html('<div class="device-selector">' +
                    self.i18n.active().phonebook.quickcallDevice + ' <select class="medium" id="device_quickcall"></select></div>')

                if(typeof monster.apps.auth.currentUser.quickcalldevice == 'undefined')
                    toastr.warning(self.i18n.active().phonebook.setquickcallDevice, '', {"timeOut": 10000});
                quickcalldevice.populateDropdown(template.find('#device_quickcall'), monster.apps.auth.currentUser.quickcalldevice||'inherit', {inherit: self.i18n.active().defaultquickcalldevice});

                $("div.actions_phonebook", template).html('<button id="renew-phonebook-link" class="monster-button monster-button-primary" data-action="renew">' +
                self.i18n.active().phonebook.renew + '</button><button id="add-phonebook-link" class="monster-button monster-button-success" data-action="addd">' +
                self.i18n.active().phonebook.add +  '</button><button id="delete-phonebook-link" class="monster-button monster-button-danger" data-action="deleted">' +
                self.i18n.active().phonebook.delete + '</button><button id="upload-link" type="button" class="monster-button monster-button-success upload-action'+
                ' upload-submit"><i class="fa fa-upload"></i></button>'),

                $('.link-quickcall').css('cursor', 'hand');

                $('#select_all_bookentrys').click(function (e) {
                    $(this).closest('table').find('td input:checkbox').prop('checked', this.checked);
                });
            })
            },
            phonebookFormatDataTable: function(data) {
                var ret = [];
                return $.each(data, function() {
                    ret.push(['', this.country||'', this.name||'', this.address||'', this.zip||'', this.city||'', this||'', this.id||''])
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
            },
            phonebookAdd: function(data, callback){
                    var self = this;
                    self.callApi({
                            resource: 'phonebook.create',
                            data: {
                                    accountId: self.accountId,
                                    data: data
                            },
                            success: function(data) {
                                    callback(data.data);
                            }
                    });
            },

            phonebookUpdate: function(data, callback){
                    var self = this;
                    self.callApi({
                            resource: 'phonebook.update',
                            data: {
                                    accountId: self.accountId,
                                    data: data
                            },
                            success: function(data) {
                                    callback && callback(data.data);
                            }
                    });
            },

            phonebookDelete: function(data, callback){
                    var self = this;
                    self.callApi({
                            resource: 'phonebook.delete',
                            data: {
                                    accountId: self.accountId,
                                    phonebookId: data.id,
                                    data: {}

                            },
                            success: function(data) {
                                    callback && callback(data.data);
                            }
                    });
            }
        };
    return app
});