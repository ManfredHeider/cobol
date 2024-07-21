      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPMA3507.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DPMA3507.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:10:16   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:10:12   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jun 28 2013 14:58:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * ------------------------------------------------------------
      * AMA00020!17.07.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATEMODELL:               !
      *         !        !     ! ISO-FORMAT AUFGENOMMEN    !
      * AMA00360!20.09.17!2.3  ! Umbenennung in Einbehalt  ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           move ZW-RUNDATE-MON-A  to ZW-EINB-MM-A
           move ZW-RUNDATE-JAHR-A to ZW-EINB-JJJJ-A
           evaluate ZW-EINB-MM-A
           when 01
              move 12             to ZW-EINB-MM-A
              compute ZW-EINB-JJJJ-A = ZW-EINB-JJJJ-A - 1
              end-compute
           when 12
              continue
           when other
              compute ZW-EINB-MM-A = ZW-EINB-MM-A - 1
              end-compute
           end-evaluate
           evaluate ZW-EINB-MM-A
           when 03
           when 12
              move 31             to ZW-EINB-TT-A
           when other
              move 30             to ZW-EINB-TT-A
           end-evaluate
      *
           move ZW-EINB-TT-A      to ZW-EINB-TT-B
                                     ZW-EINB-TT-I
           move ZW-EINB-MM-A      to ZW-EINB-MM-B
                                     ZW-EINB-MM-I
           move ZW-EINB-JJJJ-A    to ZW-EINB-JJJJ-B
                                     ZW-EINB-JJJJ-I
      *
      **************************************************************

