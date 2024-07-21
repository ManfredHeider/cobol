      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.11.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5122.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5122.CPv  $
      *
      *   Rev 2.1   Nov 17 2015 08:41:02   i13az61
      *AMA00219: Erweiterung Badenia-DTA um Informationen §34c und 
      *§34
      *
      *   Rev 2.0   Nov 16 2015 09:37:14   i13az61
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: POSTVERTEILUNGSSCHLÜSSEL
      *
      *     SOURCE-NAME      :  DRMA5122
      *
      *     GENERIERUNG      :  16.11.2015/09:44:39
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00219!02.11.14!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 POSTVERTEILUNGS-DTA.
              07 SATZ-KZ                pic X.
              07 DTASATZ.
                 09 SATZART             pic XX.
                 09 VBNR                pic 9(9).
                 09 FEHLER-CODE         pic 99.
                 09 DIR-NR              pic 9(4).
                 09 POSTVERTEILUNG.
                    11 VB-VBNR          pic 9(9).
                    11 VB-NAME          pic X(83).
                    11 DIRB-VBNR        pic 9(9).
                    11 DIRB-NAME        pic X(83).
                    11 DBE-VBNR         pic 9(9).
                    11 DBE-NAME         pic X(83).
                 09 SERV-ADRESSE.
                    11 SERV-VBNR        pic 9(9).
                    11 SERV-ANREDE      pic X(20).
                    11 SERV-ZUNAME      pic X(40).
                    11 SERV-VORNAME     pic X(20).
                    11 SERV-TITEL-AKAD  pic X(10).
                    11 SERV-TITEL-ADEL  pic X(10).
                    11 SERV-AUSL-KZ     pic X(4).
                    11 SERV-PLZ         pic X(9).
                    11 SERV-ORT         pic X(40).
                    11 SERV-STR         pic X(40).
                    11 SERV-TELEFON     pic X(26).
                    11 SERV-FAX-NR      pic X(26).
                    11 SERV-HANDY       pic X(26).
                    11 SERV-EMAIL       pic X(83).
                 09 PARA34-DATEN.
                    11 PARA34C-ZULASSUNGSDATUM
                                        pic 9(8).
                    11 PARA34C-NAME     pic X(40).
                    11 PARA34C-STR      pic X(40).
                    11 PARA34C-PLZ      pic X(9).
                    11 PARA34C-ORT      pic X(40).
                    11 PARA34I-ZULASSUNGSDATUM
                                        pic 9(8).
                    11 PARA34I-NAME     pic X(40).
                    11 PARA34I-STR      pic X(40).
                    11 PARA34I-PLZ      pic X(9).
                    11 PARA34I-ORT      pic X(40).
                    11 PARA34I-REGISTERNUMMER
                                        pic X(15).
      *
      **************************************************************

