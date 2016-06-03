define(["require", "jquery", "underscore", "monster", "toastr"], function(e) {
    var t = e("jquery"),
        n = e("underscore"),
        r = e("monster"),
        i = e("toastr"),
        s = {
            requests: {},
            subscribe: {
                "phonebook.blacklist.render": "blacklistRender"
            },
            blacklistRender: function(e) {
                var n = this,
                    i = i || {},
                    s = i.parent || t(".right-content"),
                    o = t(r.template(n, "blacklist-list"));
                n.blacklistInitTable(o, function() {
                    n.blacklistBindEvents(o), s.empty().append(o)
                })
            },
            blacklistFormatListingData: function(e) {
                var t = this,
                    n = {
                        statuses: {}
                    };
                return n
            },
            blacklistBindEvents: function(e) {
                var n = this;
                e.find("#blacklist_grid tbody").on("click", ".flush-blf", function() {
                    var e = t(this).data("key");
                    n.blacklistFlush(e, function() {
                        var t = r.template(n, "!" + n.i18n.active().blacklist.flushSent, {
                            variable: e
                        });
                        i.success(t)
                    })
                })
            },
            blacklistInitTable: function(e, n) {
                var i = this,
                    s = [{
                        sTitle: i.i18n.active().blacklist.tableTitles.subscription
                    }, {
                        sTitle: i.i18n.active().blacklist.tableTitles.mwi
                    }, {
                        sTitle: i.i18n.active().blacklist.tableTitles.blf
                    }, {
                        sTitle: i.i18n.active().blacklist.tableTitles.blacklist
                    }, {
                        sTitle: i.i18n.active().blacklist.tableTitles.flush,
                        bSortable: !1,
                        sWidth: "20px",
                        fnRender: function(e) {
                            return '<a class="flush-blf" data-key="' + e.aData[e.iDataColumn] + '"href="javascript:void(0);"><i class="fa fa-refresh"></i></a>'
                        }
                    }];
                i.blacklistGetData(function(i) {
                    r.ui.table.create("blacklist", e.find("#blacklist_grid"), s, i, {
                        sDom: '<"table-custom-actions">frtlip',
                        aaSorting: [
                            [0, "desc"]
                        ]
                    }), t.fn.dataTableExt.afnFiltering.pop(), n && n()
                })
            },
            blacklistGetData: function(e) {
                var t = this;
                t.blacklistList(function(n) {
                    var r = t.blacklistFormatDataTable(n);
                    e && e(r)
                })
            },
            blacklistFormatDataTable: function(e) {
                var t = this,
                    r = [],
                    i;
                return e.hasOwnProperty("subscriptions") && n.each(e.subscriptions, function(e, t) {
                    mwi = 0, blf = 0, pr = 0, n.each(e, function(e, t) {
                        i = 0, n.each(e, function() {
                            i++
                        }), t == "message-summary" ? mwi = i : t == "dialog" ? blf = i : t == "blacklist" && (pr = i)
                    }), r.push([t, mwi, blf, pr, t])
                }), console.log(r), r
            },
            blacklistList: function(e) {
                var t = this;
                t.callApi({
                    resource: "blacklist.list",
                    data: {
                        accountId: t.accountId
                    },
                    success: function(t) {
                        e && e(t.data)
                    }
                })
            },
            blacklistFlush: function(e, t) {
                var n = this;
                n.callApi({
                    resource: "blacklist.update",
                    data: {
                        accountId: n.accountId,
                        blacklistId: e,
                        data: {
                            reset: !0
                        }
                    },
                    success: function(e) {
                        t && t(e.data)
                    }
                })
            }
        };
    return s
});