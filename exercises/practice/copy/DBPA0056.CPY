      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBPA0056.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBPA0056.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:01:32   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Feb 04 2010 10:09:52   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DBPA0056                                     *
      *    --------------------------------------------------------  *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    08.02.2010                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01000!15.06.10!2.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *   MAINFRAME EXPRESS 3.1  6.006.0061
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-28)
      *   MAPSET NAME   BPA0056
      *   DATE CREATED  05/20/2010
      *   TIME CREATED  09:36:27

      *  INPUT DATA FOR MAP BPA0056
         01 BPA0056I.
            03 FILLER                         PIC X(12).
            03 TEXT1L                         PIC S9(4) COMP.
            03 TEXT1F                         PIC X.
            03 FILLER REDEFINES TEXT1F.
               05 TEXT1A                         PIC X.
            03 FILLER                         PIC X(1).
            03 TEXT1I                         PIC X(12).
            03 ASEIFBL                        PIC S9(4) COMP.
            03 ASEIFBF                        PIC X.
            03 FILLER REDEFINES ASEIFBF.
               05 ASEIFBA                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIFBI                        PIC X(6).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER REDEFINES ESEIAKTF.
               05 ESEIAKTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(3).
            03 ASEISTRL                       PIC S9(4) COMP.
            03 ASEISTRF                       PIC X.
            03 FILLER REDEFINES ASEISTRF.
               05 ASEISTRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEISTRI                       PIC X(1).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER REDEFINES ASEIGESF.
               05 ASEIGESA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(3).
            03 SFTEXTL                        PIC S9(4) COMP.
            03 SFTEXTF                        PIC X.
            03 FILLER REDEFINES SFTEXTF.
               05 SFTEXTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 SFTEXTI                        PIC X(14).
            03 SFGESL                         PIC S9(4) COMP.
            03 SFGESF                         PIC X.
            03 FILLER REDEFINES SFGESF.
               05 SFGESA                         PIC X.
            03 FILLER                         PIC X(1).
            03 SFGESI                         PIC X(2).
            03 SFVNRL                         PIC S9(4) COMP.
            03 SFVNRF                         PIC X.
            03 FILLER REDEFINES SFVNRF.
               05 SFVNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 SFVNRI                         PIC X(15).
            03 UFELD1L                        PIC S9(4) COMP.
            03 UFELD1F                        PIC X.
            03 FILLER REDEFINES UFELD1F.
               05 UFELD1A                        PIC X.
            03 FILLER                         PIC X(1).
            03 UFELD1I                        PIC X(17).
            03 UFELD2L                        PIC S9(4) COMP.
            03 UFELD2F                        PIC X.
            03 FILLER REDEFINES UFELD2F.
               05 UFELD2A                        PIC X.
            03 FILLER                         PIC X(1).
            03 UFELD2I                        PIC X(78).

            03 ZEILEI OCCURS 12.

               05 EFCZL                       PIC S9(4) COMP.
               05 EFCZF                       PIC X.
               05 FILLER REDEFINES EFCZF.
                  10 EFCZA                       PIC X.
               05 FILLER                      PIC X(1).
               05 EFCZI                       PIC X(1).
               05 EGESL                       PIC S9(4) COMP.
               05 EGESF                       PIC X.
               05 FILLER REDEFINES EGESF.
                  10 EGESA                       PIC X.
               05 FILLER                      PIC X(1).
               05 EGESI                       PIC X(2).
               05 EVNRL                       PIC S9(4) COMP.
               05 EVNRF                       PIC X.
               05 FILLER REDEFINES EVNRF.
                  10 EVNRA                       PIC X.
               05 FILLER                      PIC X(1).
               05 EVNRI                       PIC X(15).
               05 APINL                       PIC S9(4) COMP.
               05 APINF                       PIC X.
               05 FILLER REDEFINES APINF.
                  10 APINA                       PIC X.
               05 FILLER                      PIC X(1).
               05 APINI                       PIC X(9).
               05 APZL                        PIC S9(4) COMP.
               05 APZF                        PIC X.
               05 FILLER REDEFINES APZF.
                  10 APZA                        PIC X.
               05 FILLER                      PIC X(1).
               05 APZI                        PIC X(2).
               05 AMEDATL                     PIC S9(4) COMP.
               05 AMEDATF                     PIC X.
               05 FILLER REDEFINES AMEDATF.
                  10 AMEDATA                     PIC X.
               05 FILLER                      PIC X(1).
               05 AMEDATI                     PIC X(11).
               05 AMEKZL                      PIC S9(4) COMP.
               05 AMEKZF                      PIC X.
               05 FILLER REDEFINES AMEKZF.
                  10 AMEKZA                      PIC X.
               05 FILLER                      PIC X(1).
               05 AMEKZI                      PIC X(1).
               05 AMEVONL                     PIC S9(4) COMP.
               05 AMEVONF                     PIC X.
               05 FILLER REDEFINES AMEVONF.
                  10 AMEVONA                     PIC X.
               05 FILLER                      PIC X(1).
               05 AMEVONI                     PIC X(8).

            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER REDEFINES AFCANZF.
               05 AFCANZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(79).

      *  OUTPUT DATA FOR MAP BPA0056
         01 BPA0056O REDEFINES BPA0056I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TEXT1H                         PIC X.
            03 TEXT1O                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ASEIFBH                        PIC X.
            03 ASEIFBO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ASEISTRH                       PIC X.
            03 ASEISTRO                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 SFTEXTH                        PIC X.
            03 SFTEXTO                        PIC X(14).
            03 FILLER                         PIC X(3).
            03 SFGESH                         PIC X.
            03 SFGESO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 SFVNRH                         PIC X.
            03 SFVNRO                         PIC X(15).
            03 FILLER                         PIC X(3).
            03 UFELD1H                        PIC X.
            03 UFELD1O                        PIC X(17).
            03 FILLER                         PIC X(3).
            03 UFELD2H                        PIC X.
            03 UFELD2O                        PIC X(78).

            03 ZEILEO OCCURS 12.

               05 FILLER                      PIC X(3).
               05 EFCZH                       PIC X.
               05 EFCZO                       PIC X(1).
               05 FILLER                      PIC X(3).
               05 EGESH                       PIC X.
               05 EGESO                       PIC X(2).
               05 FILLER                      PIC X(3).
               05 EVNRH                       PIC X.
               05 EVNRO                       PIC X(15).
               05 FILLER                      PIC X(3).
               05 APINH                       PIC X.
               05 APINO                       PIC X(9).
               05 FILLER                      PIC X(3).
               05 APZH                        PIC X.
               05 APZO                        PIC X(2).
               05 FILLER                      PIC X(3).
               05 AMEDATH                     PIC X.
               05 AMEDATO                     PIC X(11).
               05 FILLER                      PIC X(3).
               05 AMEKZH                      PIC X.
               05 AMEKZO                      PIC X(1).
               05 FILLER                      PIC X(3).
               05 AMEVONH                     PIC X.
               05 AMEVONO                     PIC X(8).

            03 FILLER                         PIC X(3).
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(79).

