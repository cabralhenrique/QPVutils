with bu as (
    select business_unit_key
    from ce_w.wtda_business_unit_eudw
    where channel = 'Advantage'
    and business_unit_key in (47,49,50)
), cust_core as (
    select lst.business_unit_key, lst.list_key as CORE_LIST_KEY, br.customer_scd1_key, cus.erp_level
    from ce_w.wtda_list lst
    join bu on lst.business_unit_key = bu.business_unit_key
    join ce_w.wtda_lov lst_lov on lst.list_type_key = lst_lov.reference_key
    join pdws.wtdb_customer_list br on lst.list_key = br.list_key
    join ce_w.wtdh_customer_sales cus on br.customer_scd1_key = cus.customer_scd1_key
    where lst_lov.Reference_Description = 'Net Price List'
), sold_to_with_core as (
    select
    cs.business_unit_key,  cs.customer_scd1_key,
    nvl(cc_s.core_list_key,nvl(cc_5.core_list_key,nvl(cc_4.core_list_key,nvl(cc_3.core_list_key,nvl(cc_2.core_list_key,nvl(cc_1.core_list_key,null)))))) as core_list_key,
    nvl(cc_s.customer_scd1_key,nvl(cc_5.customer_scd1_key,nvl(cc_4.customer_scd1_key,nvl(cc_3.customer_scd1_key,nvl(cc_2.customer_scd1_key,nvl(cc_1.customer_scd1_key,null)))))) as contract_customer_scd1_key,
    nvl(cc_s.erp_level,nvl(cc_5.erp_level,nvl(cc_4.erp_level,nvl(cc_3.erp_level,nvl(cc_2.erp_level,nvl(cc_1.erp_level,null)))))) as contract_erp_level
    from ce_w.wtdh_customer_sales cs
    left join cust_core cc_1 on cs.customer_master_scd1_key = cc_1.customer_scd1_key
    left join cust_core cc_2 on cs.customer_group_scd1_key = cc_2.customer_scd1_key
    left join cust_core cc_3 on cs.customer_hn3_scd1_key = cc_3.customer_scd1_key
    left join cust_core cc_4 on cs.customer_hn4_scd1_key = cc_4.customer_scd1_key
    left join cust_core cc_5 on cs.customer_hn5_scd1_key = cc_5.customer_scd1_key
    left join cust_core cc_s on cs.customer_scd1_key = cc_s.customer_scd1_key
    where cs.erp_level = 'SoldTo'
    and nvl(cc_s.core_list_key,nvl(cc_5.core_list_key,nvl(cc_4.core_list_key,nvl(cc_3.core_list_key,nvl(cc_2.core_list_key,nvl(cc_1.core_list_key,null)))))) is not null
), sales as (
    select sd.business_unit_key, sd.customer_scd1_key, sd.product_scd1_key, sum(sd.volume_delivered_local) as volume_delivered_local, sum(sd.net_sales_value_euro) as net_sales_value_euro
    from ce_w.wtfe_sales_order_dlvrd_eudw sd
    join bu on sd.business_unit_key = bu.business_unit_key
    where date_key > trunc(sysdate)-(30)
    group by sd.business_unit_key, sd.customer_scd1_key, sd.product_scd1_key
)
select sc.business_unit_key, sc.customer_scd1_key, pl.product_scd1_key, sum(volume_delivered_local), sum(net_sales_value_euro)
from pdws.wtfe_price_list pl
join sold_to_with_core sc on pl.business_unit_key = sc.business_unit_key and pl.list_key = sc.core_list_key
left join sales s on sc.business_unit_key = s.business_unit_key and sc.customer_scd1_key = s.customer_scd1_key and pl.product_scd1_key = s.product_scd1_key
where rownum < 10
group by sc.business_unit_key, sc.customer_scd1_key, pl.product_scd1_key
