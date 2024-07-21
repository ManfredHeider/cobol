      *---------------------------------------------------------------*
      *
      *     mitCopy: Zuordnung KNR <-> Berechnungsgrundlage
      *
      *     SOURCE-NAME      :  DRMA4283
      *     DMR-NAME         :  DG-OV4283
      *
      *---------------------------------------------------------------*
      * Kurzbeschreibung:
      * ================
      *    Diese Copy-Strecke ist die Zuordnung der KNR zur ent-
      *    sprechenden Berechnungsgrundlage für
      *    - die Erfolgsprovision
      *    - den Existenzgründerbonus (entfällt seit 12/2019)
      *    - den Zusatzbonus (entfällt seit 2017)
      *    - den Existenzgründer-Bonus (Neu).
      *
      *
      * Voraussetzungen:
      * ===============
      *    keine
      *
      *
      * Funktionale Beschreibung:
      * ========================
      *    Zu jeder Vertriebsorganisation ist festgehalten, wie
      *    die Berechnungsparameter abhängig von der KNR definiert
      *    sind. Bei diesen Parametern handelt es sich um
      *    - den Faktor
      *    - die zu erreichende AE-Durchschnittsgrenze
      *      bzw. AE-Summe (für den Existenzgründer-Bonus (Neu))
      *    - den EQF-Prozentwert, ab     dem eine Kürzung erfolgt
      *      (der EQF muss größer sein, damit eine Kürzung erfolgt)
      *    - den EQF-Prozentwert, bis zu dem eine Kürzung erfolgt
      *      (ist der EQF größer als dieser ergibt sich keine
      *       Auszahlung).
      *    ==> Es erfolgt eine Kürzung, wenn
      *        EQF-KUERZUNG-VON < EQF <= EQF-KUERZUNG-BIS!
      *
      *    Zurzeit gehören die Nummern 001 - 010 zur Erfolgsprovision,
      *    die Nummern 011 und 012 zum Existenzgründerbonus, die
      *    Nummern 013 und 014 zum Zusatzbonus und die Nummern 016 -
      *    025 zum Existenzgründer-Bonus (Neu).
      *
      *
      *    Die Abkürzung des Tabellennamens leitet sich wie folgt ab:
      *       KNRB: KNR <-> Berechnungsgrundlage
      *             ---     -
      *
      *
      * Schnittstellen:
      * ==============
      *    Keine besonderen Erläuterungen notwendig.
      *
      *---------------------------------------------------------------*
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00011!01.07.13!2.1  ! ÜBERNAHME AUS MIGRATION   ! O.WILHELM
      * AMA00011!18.07.13!2.2  ! ÜBERNAHME AUS MIGRATION   ! O.WILHELM
      * AMA00045!23.12.13!2.3  ! HBO-spezifische Sonderlei-! O.WILHELM
      *         !        !     ! stungen: für VG 1303: 008,!
      *         !        !     ! 009, 013 und 014 geändert !
      *         !        !     ! (AE-DS-GRENZE von 50%     !
      *         !        !     !  auf 65% erhöht:          !
      *         !        !     !  100,0 ==> 130,0          !
      *         !        !     !  125,0 ==> 162,5);        !
      *         !        !     ! wegen AMA00042:           !
      *         !        !     !       ========            !
      *         !        !     ! für VG 1302: 005, 006 und !
      *         !        !     ! 007 geändert              !
      *         !        !     ! (EQF-KUERZUNG-BIS von     !
      *         !        !     !  20,0 in 15,0 verringert) !
      * AMA00045!15.01.14!2.4  ! Logbuch eingefügt         ! O.WILHELM
      * AMA00079!26.09.14!2.5  ! Einträge für VG 1304      ! O.WILHELM
      *         !        !     ! gelöscht                  !
      * AMA00134!23.12.14!2.6  ! HBO-spezifische Sonderlei-! O.WILHELM
      *         !        !     ! stungen: für VG 1303: 008,!
      *         !        !     ! 009, 013 und 014 geändert !
      *         !        !     ! (AE-DS-GRENZE von 65%     !
      *         !        !     !  auf 80% erhöht:          !
      *         !        !     !  130,0 ==> 160,0          !
      *         !        !     !  162,5 ==> 200,0);        !
      * AMA00147!27.01.15!2.7  ! Neue Sonderleistung EZ:   ! O.WILHELM
      *         !        !     ! für VG 1301 und 1303:     !
      *         !        !     ! 016 - 023 eingefügt       !
      * AMA00188!30.04.15!2.8  ! Neue Sonderleistung EZ:   ! D.WILHELM
      *         !        !     ! für VG 1302:              !
      *         !        !     ! 016 - 023 eingefügt       !
      * AMA00246!23.12.15!2.9  ! HBO-spezifische Sonderlei-! O.WILHELM
      *         !        !     ! stungen: für VG 1303: 008,!
      *         !        !     ! 009, 013 und 014 geändert !
      *         !        !     ! (AE-DS-GRENZE von 80%     !
      *         !        !     !  auf 90% erhöht:          !
      *         !        !     !  160,0 ==> 180,0          !
      *         !        !     !  200,0 ==> 225,0);        !
      * AMA00277!19.02.16!2.10 !Existenzgründer-Zuschuss =>! O.Wilhelm
      *         !        !     !Existenzgründer-Bonus (neu)!
      * AMA00277!24.02.16!2.11 !Existenzgründer-Bonus (Neu)! O.Wilhelm
      * AMA00302!27.12.16!2.12 ! HBO-spezifische Sonderlei-! O.Wilhelm
      *         !        !     ! stungen: für VG 1303: 008,!
      *         !        !     ! 009, 013 und 014 gelöscht,!
      *         !        !     !da Besonderheiten entfallen!
      * AMA00400!26.02.18!2.13 ! Einträge für VG 1305      ! D.WILHELM
      *         !        !     ! (Allfinanz AG) eingefügt  !
      *         !        !     ! (vorerst Kopie von 1303)  !
      *         !        !     ! Dummy-Einträge wg. Verarb.!
      *         !        !     ! in UPMA4290               !
      *         !        !     !    UPMA4296               !
      * MA-1059 !12.05.22!-----! EP und EZ: Anpassungen    ! O.WILHELM
      * MA-1060 !        !     !                           !
091122* MA-1722 !09.11.22!-----! Korrektur EP für DVAG Wien! O.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      *---------------------------------------------------------------*
       01  TABKNRB-FELDER.
           05 TABKNRB-VG-MAX           pic S9(4) binary sync value 25.
       01  TABELLE-KNRB.
           05 TABKNRB-WERTE.
              10 TABKNRB-WERT-1301-001 pic X(22)
                                       value '002,00|100,0|10,0|15,0'.
              10 TABKNRB-WERT-1301-002 pic X(22)
                                       value '003,25|150,0|15,0|20,0'.
              10 TABKNRB-WERT-1301-003 pic X(22)
                                       value '003,25|200,0|15,0|20,0'.
              10 TABKNRB-WERT-1301-004 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-005 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-006 pic X(22)
                                       value '003,25|250,0|10,0|15,0'.
              10 TABKNRB-WERT-1301-007 pic X(22)
                                       value '003,25|300,0|10,0|15,0'.
              10 TABKNRB-WERT-1301-008 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-009 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-010 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-011 pic X(22)
                                       value '002,00|150,0|15,0|20,0'.
              10 TABKNRB-WERT-1301-012 pic X(22)
                                       value '002,00|200,0|15,0|20,0'.
              10 TABKNRB-WERT-1301-013 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-014 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-015 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1301-016 pic X(22)
                                       value '500,00|000,0|00,0|00,0'.
              10 TABKNRB-WERT-1301-017 pic X(22)
                                       value '500,00|100,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-018 pic X(22)
                                       value '500,00|200,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-019 pic X(22)
                                       value '500,00|300,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-020 pic X(22)
                                       value '500,00|350,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-021 pic X(22)
                                       value '500,00|400,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-022 pic X(22)
                                       value '500,00|450,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-023 pic X(22)
                                       value '500,00|500,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-024 pic X(22)
                                       value '500,00|550,0|20,0|20,0'.
              10 TABKNRB-WERT-1301-025 pic X(22)
                                       value '500,00|600,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-001 pic X(22)
091122                                 value '002,00|100,0|10,0|15,0'.
091122*                                value '003,50|150,0|10,0|15,0'.
              10 TABKNRB-WERT-1302-002 pic X(22)
                                       value '003,50|150,0|10,0|15,0'.
              10 TABKNRB-WERT-1302-003 pic X(22)
                                       value '003,50|200,0|10,0|15,0'.
              10 TABKNRB-WERT-1302-004 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-005 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-006 pic X(22)
                                       value '003,50|250,0|10,0|15,0'.
              10 TABKNRB-WERT-1302-007 pic X(22)
                                       value '003,50|300,0|10,0|15,0'.
              10 TABKNRB-WERT-1302-008 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-009 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-010 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-011 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-012 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-013 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-014 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-015 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1302-016 pic X(22)
                                       value '500,00|000,0|00,0|00,0'.
              10 TABKNRB-WERT-1302-017 pic X(22)
                                       value '500,00|100,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-018 pic X(22)
                                       value '500,00|200,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-019 pic X(22)
                                       value '500,00|300,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-020 pic X(22)
                                       value '500,00|350,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-021 pic X(22)
                                       value '500,00|400,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-022 pic X(22)
                                       value '500,00|450,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-023 pic X(22)
                                       value '500,00|500,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-024 pic X(22)
                                       value '500,00|550,0|20,0|20,0'.
              10 TABKNRB-WERT-1302-025 pic X(22)
                                       value '500,00|600,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-001 pic X(22)
                                       value '002,00|100,0|10,0|15,0'.
              10 TABKNRB-WERT-1303-002 pic X(22)
                                       value '003,25|150,0|15,0|20,0'.
              10 TABKNRB-WERT-1303-003 pic X(22)
                                       value '003,25|200,0|15,0|20,0'.
              10 TABKNRB-WERT-1303-004 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-005 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-006 pic X(22)
                                       value '003,25|250,0|10,0|15,0'.
              10 TABKNRB-WERT-1303-007 pic X(22)
                                       value '003,25|300,0|10,0|15,0'.
              10 TABKNRB-WERT-1303-008 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-009 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-010 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-011 pic X(22)
                                       value '002,00|150,0|15,0|20,0'.
              10 TABKNRB-WERT-1303-012 pic X(22)
                                       value '002,00|200,0|15,0|20,0'.
              10 TABKNRB-WERT-1303-013 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-014 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-015 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1303-016 pic X(22)
                                       value '500,00|000,0|00,0|00,0'.
              10 TABKNRB-WERT-1303-017 pic X(22)
                                       value '500,00|100,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-018 pic X(22)
                                       value '500,00|200,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-019 pic X(22)
                                       value '500,00|300,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-020 pic X(22)
                                       value '500,00|350,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-021 pic X(22)
                                       value '500,00|400,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-022 pic X(22)
                                       value '500,00|450,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-023 pic X(22)
                                       value '500,00|500,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-024 pic X(22)
                                       value '500,00|550,0|20,0|20,0'.
              10 TABKNRB-WERT-1303-025 pic X(22)
                                       value '500,00|600,0|20,0|20,0'.
              10 TABKNRB-WERT-0000-001 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-002 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-003 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-004 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-005 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-006 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-007 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-008 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-009 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-010 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-011 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-012 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-013 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-014 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-015 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-016 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-017 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-018 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-019 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-020 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-021 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-022 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-023 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-024 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-0000-025 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-001 pic X(22)
                                       value '002,00|100,0|10,0|15,0'.
              10 TABKNRB-WERT-1305-002 pic X(22)
                                       value '003,25|150,0|15,0|20,0'.
              10 TABKNRB-WERT-1305-003 pic X(22)
                                       value '003,25|200,0|15,0|20,0'.
              10 TABKNRB-WERT-1305-004 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-005 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-006 pic X(22)
                                       value '003,25|250,0|10,0|15,0'.
              10 TABKNRB-WERT-1305-007 pic X(22)
                                       value '003,25|300,0|10,0|15,0'.
              10 TABKNRB-WERT-1305-008 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-009 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-010 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-011 pic X(22)
                                       value '002,00|150,0|15,0|20,0'.
              10 TABKNRB-WERT-1305-012 pic X(22)
                                       value '002,00|200,0|15,0|20,0'.
              10 TABKNRB-WERT-1305-013 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-014 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-015 pic X(22)
                                       value '------|-----|----|----'.
              10 TABKNRB-WERT-1305-016 pic X(22)
                                       value '500,00|000,0|00,0|00,0'.
              10 TABKNRB-WERT-1305-017 pic X(22)
                                       value '500,00|100,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-018 pic X(22)
                                       value '500,00|200,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-019 pic X(22)
                                       value '500,00|300,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-020 pic X(22)
                                       value '500,00|350,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-021 pic X(22)
                                       value '500,00|400,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-022 pic X(22)
                                       value '500,00|450,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-023 pic X(22)
                                       value '500,00|500,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-024 pic X(22)
                                       value '500,00|550,0|20,0|20,0'.
              10 TABKNRB-WERT-1305-025 pic X(22)
                                       value '500,00|600,0|20,0|20,0'.
           05 redefines TABKNRB-WERTE.
              10 TABKNRB.
                 15 TABKNRB-VG               occurs 5
                                             indexed by TABKNRB-IND1.
                    20 TABKNRB-VG-GES        occurs 25
                                             indexed by TABKNRB-IND2.
                       25 TABKNRB-FAKTOR           pic 999,99.
                       25 TABKNRB-FAKTOR-ALPHA redefines TABKNRB-FAKTOR
                                                   pic X(6).
                       25                          pic X.
                       25 TABKNRB-AE-DS-GRENZE     pic 999,9.
                       25                          pic X.
                       25 TABKNRB-EQF-KUERZUNG-VON pic 99,9.
                       25                          pic X.
                       25 TABKNRB-EQF-KUERZUNG-BIS pic 99,9.
