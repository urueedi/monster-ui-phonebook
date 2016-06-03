define(["require", "jquery", "underscore", "monster", "toastr"], function(e) {
    var t = e("jquery"),
        n = e("underscore"),
        r = e("monster"),
        i = e("toastr"),
        s = {
            requests: {},
            subscribe: {
                "phonebook.speeddial.render": "speeddialRender"
            },
            speeddialRender: function(e) {
                var n = this,
                    e = e || {},
                    i = e.parent || t(".right-content"),
                    s = t(r.template(n, "speeddial-layout"));
                n.speeddialInitTable(s, function() {
                    n.speeddialBindEvents(s), i.empty().append(s)
                })
            },
            speeddialBindEvents: function(e) {
                var n = this;
                e.on("click", ".detail-link", function() {
                    var e = t(this).data("id");
                    n.speeddialRenderDetailPopup(e)
                })
            },
            speeddialRenderDetailPopup: function(e) {
                var n = this;
                n.speeddialGetDetails(e, function(e) {
                    var i = t(r.template(n, "speeddial-detail", e));
                    i.find("#close").on("click", function() {
                        s.dialog("close").remove()
                    });
                    var s = r.ui.dialog(i, {
                        title: n.i18n.active().speeddial.detailDialog.popupTitle,
                        position: ["center", 20]
                    })
                })
            },
            speeddialFormatDataTable: function(e) {
                var n = this,
                    i = [];
                return t.each(e, function() {
                    i.push([this.hasOwnProperty("error"), this.from, this.to, r.util.toFriendlyDate(this.created), this.id, this.id, this.created])
                }), i
            },
            speeddialFormatDetailData: function(e) {
                var t = this,
                    r = {
                        metadata: {},
                        errors: []
                    },
                    i = "";
                return n.each(e, function(e, n) {
                    n === "errors" ? r.errors = e : (i = t.i18n.active().speeddial.detailDialog.apiKeys.hasOwnProperty(n) ? t.i18n.active().speeddial.detailDialog.apiKeys[n] : n.replace(/_/g, " "), r.metadata[n] = {
                        friendlyKey: i,
                        value: e
                    })
                }), r
            },
            speeddialInitTable: function(e, n) {
                var i = this,
                    s, o, u = [{
                        sTitle: i.i18n.active().speeddial.tableTitles.status,
                        fnRender: function(e) {
                            return s = e.aData[0], o = s ? "thumbs-down monster-red" : "thumbs-up monster-green", '<i class="icon-' + o + '">'
                        }
                    }, {
                        sTitle: i.i18n.active().speeddial.tableTitles.from
                    }, {
                        sTitle: i.i18n.active().speeddial.tableTitles.to
                    }, {
                        sTitle: i.i18n.active().speeddial.tableTitles.date
                    }, {
                        sTitle: i.i18n.active().speeddial.tableTitles.details,
                        fnRender: function(e) {
                            return '<a href="#" class="detail-link monster-link" data-id="' + e.aData[4] + '"><i class="fa fa-eye"></i></a>'
                        }
                    }, {
                        sTitle: "ID",
                        bVisible: !1
                    }, {
                        sTitle: "Timestamp",
                        bVisible: !1
                    }];
                i.speeddialGetData(function(i) {
                    r.ui.table.create("speeddial", e.find("#speed_dial_grid"), u, i, {
                        sDom: '<"table-custom-actions">frtlip',
                        aaSorting: [
                            [3, "desc"]
                        ]
                    }), t.fn.dataTableExt.afnFiltering.pop(), n && n()
                })
            },
            speeddialGetData: function(e) {
                var t = this;
                t.callApi({
                    resource: "faxes.getLogs",
                    data: {
                        accountId: t.accountId
                    },
                    success: function(n) {
                        var r = t.speeddialFormatDataTable(n.data);
                        e && e(r)
                    }
                })
            },
            speeddialGetDetails: function(e, t) {
                var n = this;
                n.callApi({
                    resource: "faxes.getLogDetails",
                    data: {
                        accountId: n.accountId,
                        logId: e
                    },
                    success: function(e) {
                        var r = n.speeddialFormatDetailData(e.data);
                        t && t(r)
                    }
                })
            }
        };
    return s
});