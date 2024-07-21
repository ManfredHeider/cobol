      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3743.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3743.CPv  $
      *
      *   Rev 2.1   Apr 29 2014 11:46:10   t13ac20
      *AMA00059: Badenia: Sourcen
      *
      *   Rev 2.0   Apr 22 2014 15:55:20   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: POSTVERTEILUNGSSCHLÜSSEL
      *
      *     SOURCE-NAME      :  DRMA3743
      *
      *     GENERIERUNG      :  22.04.2014/16:43:53
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00059!22.04.14!2.1  ! NUR ÜBERNAHME AUS AACHEN  ! WOLFF
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
      *
      **************************************************************
