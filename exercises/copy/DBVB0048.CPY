      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0048.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBVB0048.CPv  $
      *
      *   Rev 2.1   Apr 05 2011 08:59:30   t13ex43
      *AVB01132: VB56 - Einrichten einer betreuten Direktion
      *
      *   Rev 2.0   Feb 11 2011 11:17:40   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.007.0036
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-31)
      *   MAPSET NAME   BVB0048
      *   DATE CREATED  03/22/2011
      *   TIME CREATED  16:37:27
      *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------+--------+----+---------------------------+----------
      * AVB01132!15.02.11!2.1 ! NEU                       ! KLEEMEIER
      *         !        !    !                           !

      *  INPUT DATA FOR MAP BVB0048
         01 BVB0048I.
            03 FILLER                         PIC X(12).
            03 ESTNRL                         PIC S9(4) COMP.
            03 ESTNRF                         PIC X.
            03 FILLER REDEFINES ESTNRF.
               05 ESTNRA                         PIC X.
            03 ESTNRI                         PIC X(7).
            03 ESTPRZL                        PIC S9(4) COMP.
            03 ESTPRZF                        PIC X.
            03 FILLER REDEFINES ESTPRZF.
               05 ESTPRZA                        PIC X.
            03 ESTPRZI                        PIC X(1).
            03 ADIR1L                         PIC S9(4) COMP.
            03 ADIR1F                         PIC X.
            03 FILLER REDEFINES ADIR1F.
               05 ADIR1A                         PIC X.
            03 ADIR1I                         PIC X(3).
            03 ANAME1L                        PIC S9(4) COMP.
            03 ANAME1F                        PIC X.
            03 FILLER REDEFINES ANAME1F.
               05 ANAME1A                        PIC X.
            03 ANAME1I                        PIC X(30).
            03 ASTR1L                         PIC S9(4) COMP.
            03 ASTR1F                         PIC X.
            03 FILLER REDEFINES ASTR1F.
               05 ASTR1A                         PIC X.
            03 ASTR1I                         PIC X(30).
            03 AORT1L                         PIC S9(4) COMP.
            03 AORT1F                         PIC X.
            03 FILLER REDEFINES AORT1F.
               05 AORT1A                         PIC X.
            03 AORT1I                         PIC X(30).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER REDEFINES ABTRNAMF.
               05 ABTRNAMA                       PIC X.
            03 ABTRNAMI                       PIC X(40).
            03 ASTFL                          PIC S9(4) COMP.
            03 ASTFF                          PIC X.
            03 FILLER REDEFINES ASTFF.
               05 ASTFA                          PIC X.
            03 ASTFI                          PIC X(2).
            03 ESTNR2L                        PIC S9(4) COMP.
            03 ESTNR2F                        PIC X.
            03 FILLER REDEFINES ESTNR2F.
               05 ESTNR2A                        PIC X.
            03 ESTNR2I                        PIC X(7).
            03 ESTPRZ2L                       PIC S9(4) COMP.
            03 ESTPRZ2F                       PIC X.
            03 FILLER REDEFINES ESTPRZ2F.
               05 ESTPRZ2A                       PIC X.
            03 ESTPRZ2I                       PIC X(1).
            03 AABDATL                        PIC S9(4) COMP.
            03 AABDATF                        PIC X.
            03 FILLER REDEFINES AABDATF.
               05 AABDATA                        PIC X.
            03 AABDATI                        PIC X(10).
            03 EAUSL                          PIC S9(4) COMP.
            03 EAUSF                          PIC X.
            03 FILLER REDEFINES EAUSF.
               05 EAUSA                          PIC X.
            03 EAUSI                          PIC X(1).
            03 ADIR2L                         PIC S9(4) COMP.
            03 ADIR2F                         PIC X.
            03 FILLER REDEFINES ADIR2F.
               05 ADIR2A                         PIC X.
            03 ADIR2I                         PIC X(3).
            03 ANAME2L                        PIC S9(4) COMP.
            03 ANAME2F                        PIC X.
            03 FILLER REDEFINES ANAME2F.
               05 ANAME2A                        PIC X.
            03 ANAME2I                        PIC X(30).

      *  OUTPUT DATA FOR MAP BVB0048
         01 BVB0048O REDEFINES BVB0048I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADIR1O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ANAME1O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 ASTR1O                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 AORT1O                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 ABTRNAMO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 ASTFO                          PIC X(2).
            03 FILLER                         PIC X(3).
            03 ESTNR2O                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZ2O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AABDATO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EAUSO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADIR2O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ANAME2O                        PIC X(30).

