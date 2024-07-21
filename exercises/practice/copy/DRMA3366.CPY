      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3366.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA3366.CPv  $
      *
      *   Rev 2.5   Jun 08 2020 14:30:18   t13ac18
      *AMA00476: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen - Copies
      *
      *   Rev 2.4   Oct 19 2017 14:18:58   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Apr 12 2016 11:21:18   t13ac20
      *AMA00269: USt: LUCY II - KOMA0509-Pflege-VB-Stammdaten - Cop
      *ies
      *
      *   Rev 2.2   Nov 30 2015 09:54:38   t13ac20
      *AMA00024: SEPA-Nacharbeiten - Copies/Programme Datenmodell
      *
      *   Rev 2.1   Jul 01 2013 11:51:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     VB-STAMMDATEN MIGRIERT
      *
      *     SOURCE-NAME      :  DRMA3366
      *
      *     GENERIERUNG      :  18.05.2020/17:42:35
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00024!20.11.15!2.2  ! BKTO-NR UND BLZ ENTFERNT  ! WOLFF
      * AMA00269!11.03.16!2.3  ! KNR-USTAT-TYP ERGÄNZT     ! ARRUDA
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG DEMA0164 EINBEH. ! WARZ
      * AMA00476!05.05.20!2.5  ! ERWEITERN BKTO-INH UND    ! T13AC18
      *    MA577!        !     ! KREDITINST AUF 40 STELLEN !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3366.
              07 VBNR                   pic S9(9)      binary.
              07 KTO-FKT                pic XX.
              07 EROEFF-DAT             pic X(10).
              07 SCHLIE-DAT             pic X(10).
              07 GEB-DAT                pic X(10).
              07 DIR-NR                 pic S9(4)      binary.
              07 STRUK-STUFE            pic X(4).
              07 STRUK-STUFE-TEXT       pic X(4).
              07 KNR-ST-TYP             pic S9(4)      binary.
              07 PRAXISSTUFE            pic S9(4)      binary.
              07 UE-BETREUER-VBNR       pic S9(9)      binary.
              07 D-BETREUER-VBNR        pic S9(9)      binary.
              07 DIR-BETREUER-VBNR      pic S9(9)      binary.
              07 ED-BESITZER-VBNR       pic S9(9)      binary.
              07 ANREDE                 pic X.
              07 ZUNAME                 pic X(40).
              07 VORNAME                pic X(20).
              07 TITEL-AKAD             pic X(14).
              07 TITEL-ADEL             pic X(15).
              07 LAND-KZN               pic XXX.
              07 PLZ                    pic X(5).
              07 ORT                    pic X(40).
              07 STR                    pic X(40).
              07 TELEFON-P              pic X(26).
              07 FAX-NR-P               pic X(26).
              07 TELEFON-B              pic X(26).
              07 FAX-NR-B               pic X(26).
              07 HANDY                  pic X(26).
              07 EMAIL                  pic X(83).
              07 URL                    pic X(83).
              07 UMST-NR                pic X(15).
              07 BKTO-INH               pic X(40).
              07 KREDITINST             pic X(40).
              07 IBAN                   pic X(34).
              07 BIC                    pic X(11).
              07 EINB-SATZ              pic S999V99    packed-decimal.
              07 REGISTER-NR            pic X(30).
              07 AM-AGENT-NR            pic S9(9)      binary.
              07 VERRECHNKTO-KZ         pic X.
              07 MA-ALLFI-STATUS        pic X.
              07 MA-KEINE-ERF-PROV      pic X.
              07 STRUK-STUFE-LANG       pic X(40).
              07 AVERM-KZN              pic X.
              07 MA-CKV-WECHSEL         pic X.
              07 MA-CKV-ERFPROV         pic X.
              07 SCHLIE-DAT-ORIG        pic X(10).
              07 STRUK-STUFE-ORIG       pic S9(4)      binary.
              07 MA-SCHW-PARTNER        pic X.
              07 KNR-USTAT-TYP          pic S9(4)      binary.
      *
      **************************************************************
