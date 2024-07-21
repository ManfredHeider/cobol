      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3883.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3883.CPv  $
      *
      *   Rev 2.2   Sep 22 2015 15:51:16   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Copies/Programme
      *
      *   Rev 2.1   Jul 01 2013 17:19:08   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AF1 DATEN ZUR ERST. ABR.-KONTR.EXCEL ANALOG RULER
      *
      *     SOURCE-NAME      :  DRMA3883
      *
      *     GENERIERUNG      :  22.09.2015/15:47:40
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!22.09.15!2.2  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3883.
              07 VG-LANG                pic X(10).
              07 CSV-TRENN-1            pic X.
              07 VBNR                   pic 9(7).
              07 CSV-TRENN-2            pic X.
              07 PG                     pic 99.
              07 CSV-TRENN-3            pic X.
              07 PRODSCHL               pic 999.
              07 CSV-TRENN-4            pic X.
              07 SUMME-PROV-PFL-SE      pic +9(13),99.
              07 CSV-TRENN-5            pic X.
              07 SUMME-PRO-RATA-SE      pic +9(13),99.
              07 CSV-TRENN-6            pic X.
              07 SUMME-BETRAG-0         pic +9(13),99.
              07 CSV-TRENN-7            pic X.
              07 SUMME-UST              pic +9(13),99.
              07 CSV-TRENN-8            pic X.
              07 SUMME-BETRAG-1         pic +9(13),99.
              07 CSV-TRENN-9            pic X.
              07 SUMME-PROVEINHEITEN    pic +9(8),9.
              07 CSV-TRENN-10           pic X.
              07 BUCHDAT                pic X(10).
              07 CSV-TRENN-11           pic X.
      *
      **************************************************************

