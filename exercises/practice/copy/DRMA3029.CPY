      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3029.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA3029.CPv  $
      *
      *   Rev 2.4   Sep 20 2018 15:02:38   t13ex15
      *AMA00400: Generali Allfinanz AG Copy Einbehalt
      *
      *   Rev 2.3   Oct 19 2017 14:15:36   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:14:34   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jul 01 2013 11:38:48   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: NORMIERTER ARBEITSBESTAND
      *
      *     SOURCE-NAME      :  DRMA3029
      *
      *     GENERIERUNG      :  24.08.2018/13:19:40
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00011!01.07.13!2.1  ! KALYPSO TEILPROJEKT 4     ! WOLFF
      * AMA00020!15.07.13!2.2  ! ÄNDERUNG DRMA3475         ! WOLFF
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DRMA3475         ! WARZ
      * AMA00400!06.08.18!2.4  ! ÄNDERUNG DRMA3475         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3029.
              07 DRMA3475.
                 09 FILE-KEY.
                    11 VBNR             pic 9(9).
                    11 PG               pic 99.
                    11 VERTRAGS-ID      pic X(15).
                    11 STRUK-BEGRIFF    pic X(10).
                    11 PRODSCHL         pic 999.
                    11 REIHENFOLGE      pic X(26).
                 09 BEGINN              pic X(7).
                 09 ABLAUF              pic X(7).
                 09 PROVAUS             pic S9(9)V99   packed-decimal.
                 09 PROVEINB            pic S9(9)V99   packed-decimal.
                 09 PROVBRUTTO          pic S9(9)V99   packed-decimal.
                 09 GEVO-AKT            pic 9(5).
                 09 HAFTZEIT-KZN        pic XX.
                 09 HZRBEG-VB           pic X(10).
                 09 HZREND-VB           pic X(10).
                 09 HZRBEG-PG           pic X(10).
                 09 HZREND-PG           pic X(10).
                 09 VN                  pic X(20).
                 09 VERF-ALT-NEU-KZN    pic X.
                 09 NVP                 pic S9(9)V99   packed-decimal.
                 09 BUCHDAT             pic X(10).
                 09 PRODART             pic 99.
                 09 GEL-KENNUNG         pic X(6).
      *
      **************************************************************

