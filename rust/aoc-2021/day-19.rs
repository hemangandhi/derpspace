// WIP: look near the bottom.
// After get_all_scanner_positions, the idea is to compute all the beacons
// from the first scanner and dedupe.

use std::collections::{HashMap, HashSet};
use std::convert::{From, Into};
use std::default::Default;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::str::FromStr;

const INPUT: &str = "--- scanner 0 ---
718,-681,-661
731,-542,656
801,759,591
646,-643,740
-720,-693,746
833,-766,-641
837,755,718
561,377,-667
-684,539,770
82,92,-66
-697,-884,-585
664,-548,591
-779,433,770
-871,829,-640
-102,35,-168
-797,460,802
684,430,-592
597,465,-711
721,-881,-641
-899,834,-661
-878,838,-879
-714,-691,733
-721,-810,-469
-854,-618,712
-885,-889,-506
852,711,522

--- scanner 1 ---
468,440,286
472,527,-646
583,-631,705
-736,813,-545
538,370,245
-464,740,481
623,-715,-515
-824,-748,-542
-404,637,388
-864,-596,-583
-856,-703,-486
546,-662,-497
-721,-512,532
-651,-478,676
-695,744,-648
513,-796,-446
-356,748,417
-680,-452,501
598,530,-505
565,-595,737
563,-519,584
-790,789,-620
345,433,258
508,445,-488
-26,1,-109

--- scanner 2 ---
848,498,-615
-361,331,752
359,-800,-671
-446,-430,900
-411,-447,847
392,-766,-618
-842,-621,-485
764,599,-700
854,448,751
-477,257,728
-533,260,-690
98,-92,-45
-533,375,-802
411,-705,788
820,523,885
-859,-635,-448
-353,-528,830
513,-791,-739
841,450,745
-62,-113,110
699,593,-633
-858,-545,-514
-553,460,-687
478,-653,884
-398,252,916
448,-716,786

--- scanner 3 ---
-632,665,679
-822,874,-687
619,-514,540
-787,-749,-549
-889,778,-635
-423,-719,639
622,657,-579
412,705,770
641,-424,404
576,-405,-848
639,517,-590
-941,-681,-540
571,-403,-881
-167,-58,-160
-400,-756,614
702,-415,624
599,-591,-906
-495,-741,750
-852,776,-624
-35,14,13
-569,661,752
327,616,653
634,575,-568
516,685,682
-844,669,746
-908,-628,-574

--- scanner 4 ---
763,-623,-325
419,-507,835
-501,259,-725
647,295,-365
836,545,517
-532,417,-733
818,487,463
836,-810,-304
-492,287,-817
504,-540,843
883,342,533
-31,-159,172
-407,-601,-692
-500,-446,918
710,379,-378
-411,479,800
22,-52,0
693,-787,-333
670,472,-409
-473,-470,771
-395,-749,-643
-360,-785,-687
-371,291,790
567,-520,756
-558,-377,759
-348,313,796

--- scanner 5 ---
-674,-530,-799
-850,545,-794
-490,-411,403
556,-639,-602
-123,172,-28
322,870,475
-446,908,791
-523,-529,-894
-883,601,-888
-843,557,-690
-632,-408,419
-558,800,785
529,-419,-611
443,811,414
696,-462,686
-551,-547,-722
469,806,450
-535,-276,382
570,693,-835
-18,37,-113
547,-434,683
-536,855,720
692,-410,747
685,661,-720
581,-502,-558
679,763,-821

--- scanner 6 ---
676,435,-462
56,126,-58
-552,844,-365
-287,-263,719
-384,-412,688
-333,-351,629
696,564,632
660,478,765
-360,-337,-498
499,-329,-420
687,472,-464
-286,-453,-522
743,577,839
-612,550,350
531,-343,495
624,502,-366
418,-387,-403
-281,-374,-334
-479,865,-505
624,-321,503
56,-49,77
401,-422,-350
-587,560,613
674,-398,499
-477,878,-499
-557,492,472

--- scanner 7 ---
70,13,-34
926,445,370
-717,711,-794
-638,-801,-606
688,-736,-416
-510,-864,-536
-739,-708,262
-555,-714,290
-696,-894,-578
611,-700,-441
-656,702,-574
772,377,379
519,-705,403
-614,535,736
-760,-683,288
4,-146,-121
-634,612,-710
578,-823,518
-586,723,711
480,385,-920
-591,721,678
705,-722,450
408,407,-809
385,370,-784
893,498,404
878,-717,-442

--- scanner 8 ---
587,-707,-621
137,75,120
-531,-755,-304
-565,525,-733
-361,-362,752
561,-887,-609
642,797,826
-377,553,-708
-431,-669,-378
543,485,-417
-379,548,-675
543,-313,889
731,712,943
-537,-745,-392
601,490,-395
-442,-402,836
523,-348,695
-488,505,713
-389,422,797
-398,483,577
571,-353,812
-45,-73,22
711,608,855
683,502,-406
-452,-477,752
657,-869,-680

--- scanner 9 ---
-616,724,628
-131,-30,-10
-782,-517,-700
222,-369,712
254,-425,757
-725,749,791
2,18,111
-980,-702,866
398,618,461
-781,-723,882
-830,727,-377
-772,-549,-703
383,-531,-735
674,505,-628
614,598,-731
-850,883,-282
288,-503,-780
342,-428,-804
357,649,521
427,793,487
-854,-697,725
-837,816,-380
214,-466,605
-707,-458,-702
-659,656,797
722,534,-739

--- scanner 10 ---
-436,527,-653
470,733,-783
618,-646,763
-466,444,-578
-755,301,887
-711,-423,427
546,-805,-386
-733,386,721
-705,-338,546
-499,-727,-644
-403,457,-538
107,10,-4
693,733,865
811,-579,731
-564,-767,-668
541,797,-784
734,-626,691
-797,-447,570
491,-747,-450
-37,-63,110
410,-770,-513
565,814,809
-563,-762,-689
-788,333,676
695,788,832
553,763,-772

--- scanner 11 ---
-850,314,-804
-843,553,-795
-812,-626,-544
-836,-602,-511
-785,-890,417
-766,-529,-641
625,482,-431
610,-705,-454
712,601,-396
749,552,915
861,552,800
835,-552,744
746,532,715
-871,532,-800
-813,-860,401
889,-552,626
-62,-92,105
784,-548,665
610,-750,-442
-857,687,923
-693,724,844
589,526,-343
-669,608,918
724,-721,-299
75,-43,-66
-860,-702,386

--- scanner 12 ---
-678,-622,848
664,538,-610
857,-495,-669
-709,-693,805
35,40,142
-728,-899,-306
172,-129,103
591,474,-479
-722,-787,-414
551,-847,758
-712,645,648
-745,777,601
-706,-676,693
-657,-809,-359
-769,692,766
-724,690,-512
837,452,691
-709,521,-410
463,-897,915
895,414,859
753,-502,-657
951,-462,-613
-730,651,-417
554,-872,890
781,428,700
676,520,-462

--- scanner 13 ---
355,373,-605
-759,-765,500
-524,299,795
-393,-536,-629
-413,371,817
350,-494,500
-417,720,-593
-411,445,-585
448,-629,533
373,-669,532
31,-57,95
374,402,-617
-481,-640,-609
826,-854,-360
798,-869,-378
328,394,-450
-107,50,-38
407,605,478
-549,276,832
559,677,498
-510,-451,-540
-484,508,-589
560,608,432
-539,-826,491
749,-736,-295
-623,-743,590

--- scanner 14 ---
-29,-144,17
426,791,-568
506,383,355
-520,634,-713
-369,-448,408
-494,-877,-509
-346,-470,402
-556,220,439
-409,-636,398
885,-499,665
-456,543,-699
-449,592,-711
-56,10,-171
-498,289,280
-439,-813,-567
551,-619,-587
615,-574,-552
119,-3,-34
539,784,-659
616,-595,-547
421,350,363
-471,313,427
-358,-750,-544
820,-644,735
587,755,-504
838,-702,681
569,363,442

--- scanner 15 ---
429,481,-845
839,878,658
799,680,702
-487,-424,448
-486,-512,-441
514,-795,459
-366,-485,389
-614,825,566
-739,494,-712
625,-779,-648
-684,417,-731
806,784,714
679,-751,-803
-723,811,496
-576,-484,-431
-669,381,-684
454,478,-713
108,75,-56
-513,-460,453
492,-730,404
608,-782,-760
-32,-55,-114
404,390,-870
402,-801,351
-729,710,598
-569,-662,-456

--- scanner 16 ---
528,711,355
75,27,-58
611,317,-381
-545,617,-537
-672,718,-605
747,468,-385
-324,-550,-457
-478,-482,-466
487,-648,529
-357,-603,-491
-731,600,-490
-57,-78,71
-562,631,329
410,-601,489
656,792,411
526,766,388
894,-773,-622
-865,-475,697
-639,595,354
-372,612,355
798,257,-370
893,-707,-712
-770,-490,833
603,-668,457
-842,-512,722
877,-736,-636

--- scanner 17 ---
-493,774,286
-658,551,-932
-748,617,-931
-863,-797,-519
-527,721,319
726,-572,595
-588,-685,338
686,812,804
797,834,732
-672,588,-915
-640,-781,405
661,849,827
493,676,-525
573,-419,-675
-577,-683,375
42,167,-19
793,-576,383
453,723,-448
421,656,-641
-846,-744,-484
799,-568,658
-502,842,440
750,-420,-714
655,-289,-676
-765,-727,-426
-120,94,-139

--- scanner 18 ---
611,-899,514
411,-655,-495
412,-832,-456
789,781,-651
-447,589,887
674,-828,543
-400,723,784
719,683,-744
-16,-54,72
736,764,-686
-664,-503,-529
-972,-720,794
-514,747,849
-731,-535,-722
-559,576,-238
585,453,852
281,-737,-494
-684,-575,-645
-625,666,-239
-863,-696,886
434,441,941
528,-749,471
-596,590,-255
-887,-839,800
421,425,867

--- scanner 19 ---
539,-605,550
467,487,849
-714,-717,-651
-484,-296,819
-635,366,769
-415,545,-716
-419,418,-752
23,35,38
-487,-441,844
-714,334,882
-392,-283,801
373,543,882
462,-588,673
426,635,-571
-803,-658,-530
-822,-628,-571
367,-587,522
440,705,-680
-463,614,-767
541,603,-686
650,-477,-530
571,499,842
-741,366,864
703,-560,-582
659,-697,-557

--- scanner 20 ---
-755,649,-599
518,-399,558
724,342,-824
143,-82,-101
-383,-716,-517
-302,-707,326
752,317,-636
877,791,607
-299,-615,-602
-381,650,604
768,-655,-689
-440,535,722
-448,-710,-584
632,-686,-725
-14,-95,42
-298,-646,355
-788,683,-546
729,375,-822
723,-744,-779
560,-395,523
-249,-760,489
-567,603,621
-634,674,-478
693,763,613
898,765,582
519,-542,445

--- scanner 21 ---
-693,-506,649
-467,851,-586
443,705,-583
-650,-526,535
255,636,676
-140,24,-133
270,832,720
-3,34,58
-496,855,-647
-761,-710,-802
-630,904,-533
389,-547,-716
274,-527,-801
-666,644,543
386,-527,-916
423,-478,395
376,770,-577
-594,686,676
-689,681,717
-535,-730,-752
546,-553,342
-534,-535,645
461,854,-678
306,705,724
510,-597,430
-640,-746,-866

--- scanner 22 ---
699,804,504
625,-309,815
-860,-380,-420
-81,-4,-33
780,-743,-678
549,790,587
-516,881,-521
744,727,-474
-576,-268,736
-111,165,75
645,821,680
-543,855,776
747,-611,-631
-746,-394,-579
-519,906,-389
660,-354,768
741,710,-586
682,-295,865
-560,-216,652
-428,931,-364
-569,761,661
-802,-418,-588
-704,854,676
713,656,-431
-566,-352,543
804,-658,-513

--- scanner 23 ---
-644,719,-815
751,-786,704
-757,798,-847
696,-342,-546
811,-457,-578
766,-750,751
877,503,-763
64,153,31
637,793,666
-739,825,-815
-724,-383,-694
-838,420,834
-347,-686,865
-372,-689,733
724,828,547
892,547,-567
841,499,-636
601,881,622
-831,429,595
-717,-487,-682
-859,424,616
-757,-289,-638
805,-356,-476
-44,-3,-44
821,-673,732
-322,-777,890

--- scanner 24 ---
23,97,-18
594,-716,684
-711,-570,-446
474,887,-697
-574,655,665
-472,711,674
-385,515,-778
-765,-512,-464
538,923,-858
-774,-396,526
845,788,462
522,-442,-669
-393,482,-863
99,-12,-136
-695,-367,-399
491,-656,591
-738,-347,645
-466,499,643
-472,495,-715
629,916,-745
940,808,306
842,795,373
517,-463,-668
557,-487,-733
-812,-350,489
483,-764,655

--- scanner 25 ---
390,425,-391
414,568,-305
305,544,-397
760,-477,721
-366,674,-314
92,-51,6
657,-417,814
390,525,957
-492,-734,670
-611,570,497
-490,-540,-699
-419,719,-369
-499,550,618
734,-670,-538
-458,-664,526
-432,-693,701
-4,93,114
694,-646,-603
460,455,916
-560,-655,-666
768,-369,785
453,332,944
-575,552,465
729,-691,-539
-477,-580,-636
-334,885,-347

--- scanner 26 ---
-617,465,-402
581,676,685
702,-580,636
608,621,851
-506,-741,-499
690,660,-394
611,-594,572
-412,754,534
660,-582,565
823,-789,-471
3,84,43
859,-725,-310
-28,-103,134
-423,784,617
694,566,704
-657,478,-339
634,693,-342
852,-634,-423
-809,-784,610
-527,-746,-500
-745,-726,555
-542,548,-372
-411,818,605
-682,-749,474
719,653,-476
-551,-706,-435

--- scanner 27 ---
471,876,833
-863,-656,869
592,697,-698
-914,-399,-385
-485,518,-510
664,677,-582
335,-399,-509
-743,596,535
-892,-393,-362
-660,-672,822
-767,556,600
418,-515,418
-549,576,-455
512,-398,-548
430,-268,-547
328,900,747
-76,96,-74
268,-598,461
447,985,741
-612,553,-374
-726,-389,-425
-788,561,599
-718,-726,804
632,744,-775
339,-661,490

--- scanner 28 ---
640,-750,-777
-546,-774,-852
-382,-853,-815
-473,711,401
801,788,-826
-427,614,498
592,-807,701
-396,865,-681
-160,-18,-17
-454,-347,762
592,875,346
-368,870,-660
-379,-316,574
581,-801,-838
-389,874,-442
756,-773,801
753,795,-668
537,-812,821
639,768,291
21,73,-77
815,871,-800
-426,-331,732
-403,581,474
671,-847,-926
-570,-848,-736
721,808,372

--- scanner 29 ---
461,-674,-716
-509,-792,644
460,-731,-659
-899,612,475
-925,637,496
-897,643,680
-469,344,-590
552,-695,741
367,-728,-717
261,553,813
-792,-634,-649
-806,-740,-573
-396,329,-469
521,-659,605
265,588,855
-451,-606,621
391,583,-551
-880,-640,-659
408,475,854
-476,-794,649
417,496,-579
610,-749,650
405,555,-451
-571,284,-545
-166,-83,37
22,-49,137";

type Real3 = (i32, i32, i32);

fn dist((vx, vy, vz): Real3, (wx, wy, wz): Real3) -> i32 {
  (vx - wx) * (vx - wx) + (vy - wy) * (vy - wy) + (vz - wz) * (vz - wz)
}

struct Scanner(u8, HashMap<i32, Vec<(Real3, Real3)>>);

fn all_pairings<'a, T: Copy + 'static>(
  left: impl Iterator<Item=&'a (T, T)>,
  right: impl Iterator<Item=&'a (T, T)> + Clone) -> Vec<Vec<(T, T)>> {
  let mut r = vec![];
  for &(l1, l2) in left {
    for &(r1, r2) in right.clone() {
      r.push(vec![(l1, r1), (l2, r2)]);
      r.push(vec![(l1, r2), (l2, r1)]);
    }
  }
  r
}

fn find_other_from_pair<'a, T: Eq>(
  needle: &'a T,
  haystack: &'a Vec<(T, T)>,
  other_needles: &'a Vec<(T, T)>,
  is_first: bool) -> Option<&'a T> {
  haystack.into_iter()
          .filter_map(|(l, r)| {
	     if *l == *needle {
	       Option::Some(r)
             } else if *r == *needle {
	       Option::Some(l)
             } else {
	       Option::None
	     }
	  })
	  .filter(|t| {
	    other_needles.into_iter()
	                 .find(|&o| if is_first { o.0 == **t } else { o.1 == **t })
			 .is_none()
	  })
	  .next()
}

fn is_valid_mapping(mapping: &Vec<(Real3, Real3)>) -> bool {
  for &(x1, y1) in mapping {
    for &(x2, y2) in mapping {
      if dist(x1, x2) != dist(y1, y2) {
        return false;
      }
    }
  }
  return true;
}

impl Scanner {
  fn overlapping_points(&self, other: &Scanner) -> Vec<(Real3, Real3)> {
    fn extend_overlap(xs: &Scanner, ys: &Scanner, mapping: &Vec<(Real3, Real3)>) -> Option<Vec<Vec<(Real3, Real3)>>> {
      let mut exts = vec![];
      for (dx, x_points) in xs.1.iter() {
        if let Option::Some(y_points) = ys.1.get(dx) {
	  for (map_x, map_y) in mapping.into_iter() {
	    if let Option::Some(next_x) = find_other_from_pair(map_x, x_points, mapping, true) {
	      if let Option::Some(next_y) = find_other_from_pair(map_y, y_points, mapping, false) {
	        let mut new_map = mapping.clone();
		new_map.push((*next_x, *next_y));
		if is_valid_mapping(&new_map) {
		  exts.push(new_map);
                }
              }
            }
	  }
        }
      }
      if exts.len() == 0 {
        Option::None
      } else {
        Option::Some(exts)
      }
    }

    let mut mapping_bag = vec![];
    for (d, xs) in self.1.iter() {
      if let Option::Some(ys) = other.1.get(d) {
        mapping_bag.append(&mut all_pairings(xs.iter(), ys.iter()));
      }
    }
    let mut maps_growing = true;
    while maps_growing {
      let next_bag: Vec<Vec<(Real3, Real3)>> = mapping_bag.iter()
                                                     .flat_map(|m| extend_overlap(self, other, m)
						                     .unwrap_or(vec![])
								     .into_iter())
				                     .collect();
      maps_growing = next_bag != mapping_bag;
      mapping_bag = next_bag;
    }
    mapping_bag.into_iter()
               .max_by_key(|v| v.len())
	       .unwrap_or(vec![])
  }
}


#[derive(Debug)]
enum ScannerParseError{
  BadIdLine,
  BadPoint
}

impl FromStr for Scanner {
  type Err = ScannerParseError;

  fn from_str(s: &str) -> Result<Scanner, ScannerParseError> {
    let mut lines = s.split('\n');
    if let Option::Some(scan_id_line) = lines.next() {
      let id: u8 = scan_id_line
                     .strip_prefix("--- scanner ")
		     .ok_or(ScannerParseError::BadIdLine)?
                     .strip_suffix(" ---")
		     .ok_or(ScannerParseError::BadIdLine)?
		     .parse()
		     .map_err(|_e| ScannerParseError::BadIdLine)?;
      let points: Vec<Real3> = lines
        .map(|line| -> Result<Real3, ScannerParseError> {
          let mut coords = line.split(',');
	  Result::Ok((
	    coords.next().unwrap_or("").parse().map_err(|_e| ScannerParseError::BadPoint)?,
	    coords.next().unwrap_or("").parse().map_err(|_e| ScannerParseError::BadPoint)?,
	    coords.next().unwrap_or("").parse().map_err(|_e| ScannerParseError::BadPoint)?,
	  ))
        })
	.collect::<Result<Vec<Real3>, ScannerParseError>>()?;
      let mut dist_map = HashMap::new();
      for &x in points.iter() {
        for &y in points.iter() {
	  dist_map.entry(dist(x, y)).or_insert(vec![]).push((x, y));
        }
      }
      Result::Ok(Scanner(id, dist_map))
    } else {
      Result::Err(ScannerParseError::BadIdLine)
    }
  }
}

struct AdjoinSqrtHalf<T>(T, T);

impl<T: From<u8>> AdjoinSqrtHalf<T> {
  fn sqrt_half() -> Self {
    AdjoinSqrtHalf(0.into(), 1.into())
  }

  fn scalar(t: T) -> Self {
    AdjoinSqrtHalf(t, 0.into())
  }
}

impl<T: Clone> Clone for AdjoinSqrtHalf<T> {
  fn clone(&self) -> Self {
    AdjoinSqrtHalf(self.0.clone(), self.1.clone())
  }
}

impl<T: Copy> Copy for AdjoinSqrtHalf<T> {}

impl<T> Mul for AdjoinSqrtHalf<T>
where T: Copy + Mul<T, Output = T> + Add<T, Output = T> + From<u8> + Div<T, Output = T>{
  type Output = AdjoinSqrtHalf<T>;

  fn mul(self, other: Self) -> Self {
    AdjoinSqrtHalf(self.0 * other.0 + (self.1 * other.1)/(2.into()), self.0 * other.1 + self.1 * other.0)
  }
}

impl<T: Copy + Add<T, Output = T>> Add for AdjoinSqrtHalf<T> {
  type Output = AdjoinSqrtHalf<T>;

  fn add(self, other: Self) -> Self {
    AdjoinSqrtHalf(self.0 + other.0, self.1 + other.1)
  }
}

impl<T: Copy + Sub<T, Output = T>> Sub for AdjoinSqrtHalf<T> {
  type Output = AdjoinSqrtHalf<T>;

  fn sub(self, other: Self) -> Self {
    AdjoinSqrtHalf(self.0 - other.0, self.1 - other.1)
  }
}


impl<T: Default> From<T> for AdjoinSqrtHalf<T> {
  fn from(item: T) -> AdjoinSqrtHalf<T> {
    AdjoinSqrtHalf(item, T::default())
  }
}

impl<T: Neg<Output = T>> Neg for AdjoinSqrtHalf<T> {
  type Output = AdjoinSqrtHalf<T>;

  fn neg(self) -> Self {
    AdjoinSqrtHalf(-self.0, -self.1)
  }
}

#[derive(Clone, Copy)]
struct RightAngleQuaternion(AdjoinSqrtHalf<f64>, AdjoinSqrtHalf<f64>, AdjoinSqrtHalf<f64>, AdjoinSqrtHalf<f64>);

impl Mul for RightAngleQuaternion {
  type Output = RightAngleQuaternion;

  fn mul(self, other: RightAngleQuaternion) -> RightAngleQuaternion {
    let RightAngleQuaternion(a1, b1, c1, d1) = self;
    let RightAngleQuaternion(a2, b2, c2, d2) = other;
    RightAngleQuaternion(
      a1 * a2 - b1 * b2 - c1 * c2 - d1 * d1,
      a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2,
      a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2,
      a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
    )
  }
}

impl RightAngleQuaternion {
  fn invert(&self) -> RightAngleQuaternion {
    RightAngleQuaternion (
      self.0,
      -self.1,
      -self.2,
      -self.3
    )
  }

  fn all_axis_rotations() -> Vec<RightAngleQuaternion> {
    let facing_axes: [RightAngleQuaternion; 6] = [
      // No rotation -- face +x
      RightAngleQuaternion(
        AdjoinSqrtHalf::scalar(1.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0)),
      // Rotate 90 along y -- face +z
      RightAngleQuaternion(
        AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0)),
      // Rotate 270 along y -- face -z
      RightAngleQuaternion(
        -AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0)),
      // Rotate 90 along z -- face +y
      RightAngleQuaternion(
        AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::sqrt_half()),
      // Rotate 180 along z -- face -x
      RightAngleQuaternion(
        AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(1.0)),
      // Rotate 270 along z -- face -y
      RightAngleQuaternion(
        -AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::sqrt_half()),
    ];
    let rotate_in_axis: [RightAngleQuaternion; 4] = [
      // No rotation
      RightAngleQuaternion(
        AdjoinSqrtHalf::scalar(1.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0)),
      // Rotate 90 along x
      RightAngleQuaternion(
        AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0)),
      // Rotate 180 along x
      RightAngleQuaternion(
        AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(1.0), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0)),
      // Rotate 270 along x
      RightAngleQuaternion(
        -AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::sqrt_half(), AdjoinSqrtHalf::scalar(0.0), AdjoinSqrtHalf::scalar(0.0)),
    ];
    facing_axes.iter()
               .flat_map(|f| rotate_in_axis.iter()
	                                   .map(move |r| (*r) * (*f)))
	       .collect()
  }
}

// A beacon found by dest positioned relative to source.
impl Mul<Real3> for RightAngleQuaternion {
  type Output = Real3;

  fn mul(self, other: Real3) -> Real3 {
    let inverse = self.invert();
    let other_nion = RightAngleQuaternion(
      0f64.into(),
      (other.0 as f64).into(),
      (other.1 as f64).into(),
      (other.2 as f64).into()
    );
    let RightAngleQuaternion(_a, x, y, z) = (self * other_nion) * inverse;
    (x.0 as i32, y.0 as i32, z.0 as i32)
  }
}

// TODO: just Copy this? It's like 80 bytes?
struct ScannerPosition {
  source: u8,
  dest: u8,
  translation: Real3,
  rotation: RightAngleQuaternion
}

impl ScannerPosition {
  fn relative_position(x: &Scanner, y: &Scanner) -> Option<Self> {
    let mapping = x.overlapping_points(y);
    if mapping.len() < 12 {
      return Option::None;
    }
    for rotation in RightAngleQuaternion::all_axis_rotations() {
      let (x2, y2, z2) = rotation.invert() * mapping[0].1;
      let (x1, y1, z1) = mapping[0].0;
      let position = ScannerPosition {
        source: x.0,
	dest: y.0,
	translation: (x1 - x2, y1 - y2, z1 - z2),
	rotation
      };
      if mapping.iter().all(|(s, d)| (&position) * (*d) == *s) {
        return Option::Some(position);
      }
    }
    Option::None
  }

  fn invert(&self) -> ScannerPosition {
    let (x, y, z) = self.rotation * self.translation;
    ScannerPosition {
      source: self.dest,
      dest: self.source,
      translation: (-x, -y, -z),
      rotation: self.rotation.invert()
    }
  }
}

impl<'a> Mul for &'a ScannerPosition {
  type Output = Option<ScannerPosition>;
  fn mul(self, other: &'a ScannerPosition) -> Option<ScannerPosition> {
    if self.dest != other.source {
      return Option::None;
    }
    let (x1, y1, z1) = self.translation;
    let (x2, y2, z2) = self.rotation.invert() * other.translation;
    Option::Some(ScannerPosition {
      source: self.source,
      dest: other.dest,
      translation: (x1 + x2, y1 + y2, z1 + z2),
      rotation: self.rotation * other.rotation
    })
  }
}

impl<'a> Mul<Real3> for &'a ScannerPosition {
  type Output = Real3;

  fn mul(self, other: Real3) -> Real3 {
    let (x, y, z) = self.rotation.invert() * other;
    (self.translation.0 + x, self.translation.1 + y, self.translation.2 + z)
  }
}

fn get_all_scanner_positions(scanners: &Vec<Scanner>) -> HashMap<u8, HashMap<u8, ScannerPosition>> {
  let mut graph = Hashmap::new();
  for src in scanners {
    for dest in scanners {
      if src.0 == dest.0 {
        continue;
      }
      if let Option::Some(rel_pos) = relative(src, dest) {
        graph.entry(src.0).or_insert(HashMap::new()).insert(dest.0, rel_pos);
      }
    }
  }
  todo!("The idea is to get all the positions relative to one scanner, probably the first in the Vec");
  return graph;
}

fn main() {
  let scanners: Vec<Scanner> = INPUT.split("\n\n")
                                    .filter(|line| line.len() > 0)
				    .map(|line| line.parse().unwrap())
				    .collect();
}