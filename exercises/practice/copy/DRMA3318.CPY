      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3318.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA3318.CPv  $
      *
      *   Rev 2.4   Jun 08 2020 14:29:38   t13ac18
      *AMA00476: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen - Copies
      *
      *   Rev 2.3   Oct 20 2017 09:41:08   t13ac19
      *AMA00360: 3. Statistik-Kategorie Folge Neu - Copys OWI
      *
      *   Rev 2.2   May 11 2016 13:19:24   t13ac20
      *AMA00269: USt: LUCY II - KOMA0509-Pflege-VB-Stammdaten - Cop
      *ies/Programme
      *
      *   Rev 2.1   Jul 01 2013 11:46:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     VB-Stammdaten DVAG ab 09/07 (per File-Transfer)
      *
      *     SOURCE-NAME      :  DRMA3318
      *
      *     GENERIERUNG      :  06.05.2020/15:52:18
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00269!11.05.16!2.2  ! KNR-USTAT-TYP,            ! WOLFF
      *         !        !     ! USTAT-TYP-BEZEICH-LANG    !
      *         !        !     ! ERGÄNZT                   !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0164 EINBEH. ! O.WILHELM
      * AMA00476!05.05.20!2.4  ! EINFÜGEN BKTOINH-LANG     ! T13AC18
      *    MA577!        !     ! UND GELDINST-LANG         !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3318.
              07 SATZ-KZ                pic X.
              07 VB-NR                  pic X(10).
              07 UEBETREUER-VB-NR       pic X(10).
              07 DBETREUER-VB-NR        pic X(10).
              07 DIRBETREUER-VB-NR      pic X(10).
              07 FUNKTION               pic XX.
              07 EROEFF-DAT             pic 9(8).
              07 SCHLIE-DAT             pic 9(8).
              07 ANREDE                 pic 9.
              07 ZUNAME                 pic X(40).
              07 VORNAME                pic X(20).
              07 TITEL-AKAD             pic X(14).
              07 TITEL-ADEL             pic X(15).
              07 AUSL-KZ                pic XXX.
              07 PLZ                    pic X(5).
              07 ORT                    pic X(40).
              07 STR                    pic X(40).
              07 GEBDAT                 pic 9(8).
              07 TELEFON                pic X(26).
              07 FAX-NR                 pic X(26).
              07 EINB-SATZ              pic S9(4)V9    packed-decimal.
              07 BKTOINHORT             pic X(25).
              07 BKTONR                 pic S9(11)     packed-decimal.
              07 GELDINST               pic X(22).
              07 BLZ                    pic S9(8)      binary.
              07 STRUK-DIR              pic X(4).
              07 STRUK-STUFE            pic X(4).
              07 KNR-ST-TYP             pic S9(4)      binary.
              07 PRAXISSTUFE            pic S9(4)      binary.
              07 STRUK-STUFE-TEXT       pic X(4).
              07 HANDY                  pic X(26).
              07 EMAIL                  pic X(83).
              07 URL                    pic X(83).
              07 UMST-NR                pic X(15).
              07 REGISTER-NR            pic X(30).
              07 AM-AGENT-NR            pic S9(9)      binary.
              07 VERRECHNKTO-KZ         pic X.
              07 MA-ALLFI-STATUS        pic X.
              07 MA-KEINE-ERF-PROV      pic X.
              07 STRUK-STUFE-LANG       pic X(40).
              07 AVERM-KZN              pic X.
              07 MA-CKV-WECHSEL         pic X.
              07 MA-CKV-ERFPROV         pic X.
              07 MA-SCHW-PARTNER        pic X.
              07 IBAN                   pic X(34).
              07 BIC                    pic X(11).
              07 KNR-USTAT-TYP          pic 9(4).
              07 USTAT-TYP-BEZEICH-LANG pic X(25).
              07 BKTOINH-LANG           pic X(40).
              07 GELDINST-LANG          pic X(40).
      *
      **************************************************************
