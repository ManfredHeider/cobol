      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2672.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRMA2672.CPv  $
      *
      *   Rev 2.7   Jun 18 2020 17:05:30   t13ac18
      *AMA00480: One Company - Umsetzung der Stufe 3 AM-Leben -> Ge
      *nerali Deutschland Lebensversicherung (Abrechnung)
      *
      *   Rev 2.6   Dec 11 2019 11:36:42   t13ac18
      *AMA00434: One Company: Umsetzung AP1 und AP6
      *
      *   Rev 2.5   Sep 20 2018 15:04:22   t13ex15
      *AMA00400: Generali Allfinanz AG Copy Einbehalt
      *
      *   Rev 2.4   Jun 12 2018 09:43:04   t13ac18
      *AMA00400: Generali Allfinanz AG - Copies DWH
      *
      *   Rev 2.3   Aug 19 2014 15:33:26   t13ac20
      *AMA00085: Projekt Integration FVD: techn. Konzeption "Rückst
      *.-Abr.": Copies
      *
      *   Rev 2.2   Oct 14 2013 10:14:16   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jul 01 2013 11:36:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *
      *     mitCopy: Zuordnung der PG-Position im Anschreiben
      *
      *     SOURCE-NAME      :  DRMA2672
      *     DMR-NAME         :  DG-OV2672
      *
      *---------------------------------------------------------------*
      * Kurzbeschreibung:
      * ================
      *    Diese Copy-Strecke ist die Referenztabelle, an welcher
      *    Stelle die Haftungsvolumen der einzelnen Produktgeber
      *    in den Anschreiben der Einbehaltsabrechnung ausgegeben
      *    werden.
      *
      *
      * Voraussetzungen:
      * ===============
      *    Die Referenztabelle korrespondiert mit DGMA3824, d.h.
      *    eine höhere Position als 8 darf es derzeit nicht geben.
      *    (Das Druckprogramm zum Anschreiben kann derzeit max.
      *     8 Produktgeberzeilen andrucken, damit es nicht zu einem
      *     Seitenwechsel kommt.)
      *
      *
      * Funktionale Beschreibung:
      * ========================
      *    Zu jeder Vertriebsorganisation ist festgehalten, an welcher
      *    Position im Anschreiben zur Einbehaltsabrechnung der
      *    jeweilige Produktgeber angedruckt werden soll. Zusätzlich
      *    ist über PGBEZ-IND angegeben, mit welchem Index auf die
      *    Referenztabelle der Gesellschaften zugegriffen werden
      *    muss. Die Werte 'F1' und 'F2' stehen für fixe Bezeichnungen,
      *    die beim Erstellen der Anschreiben vergeben werden.
      *
      *    Über PGBEZ-KZN wird angegeben, ob die Kurz- oder Lang-
      *    bezeichnung ausgegeben werden soll.
      *
      *    Bei der höchsten nicht belegten POSITION wird PGBEZ-IND
      *    mit '--' (als Ende-Kriterium) gefüllt. Bei gleicher
      *    POSITION muss PGBEZ-IND immer identisch sein.
      *
      *
      *    Die Abkürzung des Tabellennamens leitet sich wie folgt ab:
      *       PGPAN: ProduktGeberPosition im ANschreiben
      *              -      -    -           --
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
      * AMA00020!12.09.13!2.2  ! GES DURCH PG ERSETZT      ! WOLFF
      * AMA00085!19.08.14!2.3  ! EINTRÄGE 1304 GELÖSCHT    ! WOLFF
      * AMA00400!26.02.18!2.4  ! Einträge für VG 1305      ! D.WILHELM
      *         !        !     ! (Allfinanz AG) eingefügt  !
      *         !        !     ! Dummy-Einträge wg. Verarb.!
      *         !        !     ! in PMMA0999               !
      *         !        !     !    PMMA3492               !
      *         !        !     !    PMMA3506               !
      * AMA00400!06.08.18!2.5  ! Friwi Einb Ges. 21        ! WARZ
      * AMA00434!28.11.19!2.6  ! Wegen Änderung der Ges.-  ! D.WILHELM
      *         !        !     ! bezeichnung für 10 bei    !
      *         !        !     ! OneCompany Änderung auf K !
      * AMA00480!25.06.20!2.7  ! Wegen Änderung der Ges.-  ! D.WILHELM
      *         !        !     ! bezeichnung für 20 und 50 !
      *         !        !     ! OneCompany Änderung auf K !
      * MA-1214 !21.06.21!2.8  ! Neuer PG 55               ! D.WILHELM
      *         !        !     ! Erweiterung von 11 auf 12 !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      *---------------------------------------------------------------*
       01  TABPGPAN-FELDER.
210621     05 TABPGPAN-VG-PG-MAX   pic S9(4) binary sync value 12.
       01  TABELLE-PGPAN.
           05 TABPGPAN-WERTE.
              10 TABPGPAN-WERT-1301-001 pic X(10) value '20|01|20|K'.
              10 TABPGPAN-WERT-1301-002 pic X(10) value '10|02|10|K'.
              10 TABPGPAN-WERT-1301-003 pic X(10) value '50|03|50|K'.
              10 TABPGPAN-WERT-1301-004 pic X(10) value '70|04|70|L'.
              10 TABPGPAN-WERT-1301-005 pic X(10) value '40|05|40|K'.
              10 TABPGPAN-WERT-1301-006 pic X(10) value '48|06|48|L'.
              10 TABPGPAN-WERT-1301-007 pic X(10) value '91|07|91|L'.
              10 TABPGPAN-WERT-1301-008 pic X(10) value '25|08|F1| '.
              10 TABPGPAN-WERT-1301-009 pic X(10) value '26|08|F1| '.
210621        10 TABPGPAN-WERT-1301-010 pic X(10) value '39|08|F1| '.
210621        10 TABPGPAN-WERT-1301-011 pic X(10) value '55|08|F1| '.
210621        10 TABPGPAN-WERT-1301-012 pic X(10) value '--|09|--| '.
              10 TABPGPAN-WERT-1302-001 pic X(10) value '25|01|25|L'.
              10 TABPGPAN-WERT-1302-002 pic X(10) value '39|02|39|L'.
              10 TABPGPAN-WERT-1302-003 pic X(10) value '26|03|26|L'.
210621        10 TABPGPAN-WERT-1302-004 pic X(10) value '55|04|55|K'.
              10 TABPGPAN-WERT-1302-005 pic X(10) value '20|05|F2| '.
              10 TABPGPAN-WERT-1302-006 pic X(10) value '10|05|F2| '.
              10 TABPGPAN-WERT-1302-007 pic X(10) value '50|05|F2| '.
              10 TABPGPAN-WERT-1302-008 pic X(10) value '70|05|F2| '.
              10 TABPGPAN-WERT-1302-009 pic X(10) value '40|05|F2| '.
              10 TABPGPAN-WERT-1302-010 pic X(10) value '48|05|F2| '.
              10 TABPGPAN-WERT-1302-011 pic X(10) value '91|05|F2| '.
              10 TABPGPAN-WERT-1302-012 pic X(10) value '--|06|--| '.
              10 TABPGPAN-WERT-1303-001 pic X(10) value '20|01|20|K'.
              10 TABPGPAN-WERT-1303-002 pic X(10) value '10|02|10|K'.
              10 TABPGPAN-WERT-1303-003 pic X(10) value '50|03|50|K'.
              10 TABPGPAN-WERT-1303-004 pic X(10) value '70|04|70|L'.
              10 TABPGPAN-WERT-1303-005 pic X(10) value '40|05|40|K'.
              10 TABPGPAN-WERT-1303-006 pic X(10) value '48|06|48|L'.
              10 TABPGPAN-WERT-1303-007 pic X(10) value '91|07|91|L'.
              10 TABPGPAN-WERT-1303-008 pic X(10) value '--|08|--| '.
              10 TABPGPAN-WERT-1303-009 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-1303-010 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-1303-011 pic X(10) value '--|--|--| '.
210621        10 TABPGPAN-WERT-1303-012 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-001 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-002 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-003 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-004 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-005 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-006 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-007 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-008 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-009 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-010 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-0000-011 pic X(10) value '--|--|--| '.
210621        10 TABPGPAN-WERT-0000-012 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-1305-001 pic X(10) value '20|01|20|K'.
              10 TABPGPAN-WERT-1305-002 pic X(10) value '21|02|21|L'.
              10 TABPGPAN-WERT-1305-003 pic X(10) value '10|03|10|K'.
              10 TABPGPAN-WERT-1305-004 pic X(10) value '50|04|50|K'.
              10 TABPGPAN-WERT-1305-005 pic X(10) value '70|05|70|L'.
              10 TABPGPAN-WERT-1305-006 pic X(10) value '40|06|40|K'.
              10 TABPGPAN-WERT-1305-007 pic X(10) value '48|07|48|L'.
              10 TABPGPAN-WERT-1305-008 pic X(10) value '91|08|91|L'.
              10 TABPGPAN-WERT-1305-009 pic X(10) value '--|09|--| '.
              10 TABPGPAN-WERT-1305-010 pic X(10) value '--|--|--| '.
              10 TABPGPAN-WERT-1305-011 pic X(10) value '--|--|--| '.
210621        10 TABPGPAN-WERT-1305-012 pic X(10) value '--|--|--| '.
           05 redefines TABPGPAN-WERTE.
              10 TABPGPAN.
                 15 TABPGPAN-VG              occurs 5
                                             indexed by TABPGPAN-IND1.
210621              20 TABPGPAN-VG-PG        occurs 12
                                             indexed by TABPGPAN-IND2.
                       25 TABPGPAN-PG        pic 99.
                       25                    pic X.
                       25 TABPGPAN-POSITION  pic 99.
                       25                    pic X.
                       25 TABPGPAN-PGBEZ-IND pic 99.
                       25 TABPGPAN-PGBEZ-IND-ALPHA redefines
                          TABPGPAN-PGBEZ-IND pic XX.
                       25                    pic X.
                       25 TABPGPAN-PGBEZ-KZN pic X.

