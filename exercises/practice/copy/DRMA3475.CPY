      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3475.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA3475.CPv  $
      *
      *   Rev 2.4   Sep 20 2018 15:03:00   t13ex15
      *AMA00400: Generali Allfinanz AG Copy Einbehalt
      *
      *   Rev 2.3   Oct 19 2017 14:19:40   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:15:28   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jul 01 2013 17:14:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     MITCOPY: NORMIERTER ARBEITSBESTAND
      *
      *     SOURCE-NAME      :  DRMA3475
      *
      *     GENERIERUNG      :  24.08.2018/08:32:07
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!15.07.13!2.1  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL               !
      * AMA00360!20.09.17!2.2  ! DEMA0111 02 GEÄNDERT AUF  ! WARZ
      *         !        !     ! 05 FÜR GEVO-AKT (DGV)     !
      *         !        !     ! ÄNDERUNG DEMA0128         !
      *         !        !     ! ÄNDERUNG DEMA0160 EINBEH. !
      * AMA00400!06.08.18!2.3  ! DEMA0496 01 NEU           ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3475.
              07 FILE-KEY.
                 09 VBNR                pic 9(9).
                 09 PG                  pic 99.
                 09 VERTRAGS-ID         pic X(15).
                 09 STRUK-BEGRIFF       pic X(10).
                 09 PRODSCHL            pic 999.
                 09 REIHENFOLGE         pic X(26).
              07 BEGINN                 pic X(7).
              07 ABLAUF                 pic X(7).
              07 PROVAUS                pic S9(9)V99   packed-decimal.
              07 PROVEINB               pic S9(9)V99   packed-decimal.
              07 PROVBRUTTO             pic S9(9)V99   packed-decimal.
              07 GEVO-AKT               pic 9(5).
              07 HAFTZEIT-KZN           pic XX.
              07 HZRBEG-VB              pic X(10).
              07 HZREND-VB              pic X(10).
              07 HZRBEG-PG              pic X(10).
              07 HZREND-PG              pic X(10).
              07 VN                     pic X(20).
              07 VERF-ALT-NEU-KZN       pic X.
              07 NVP                    pic S9(9)V99   packed-decimal.
              07 BUCHDAT                pic X(10).
              07 PRODART                pic 99.
              07 GEL-KENNUNG            pic X(6).
      *
      **************************************************************

