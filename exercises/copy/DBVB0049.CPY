      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0049.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBVB0049.CPv  $
      *
      *   Rev 2.1   Apr 05 2011 09:12:30   t13ex43
      *AVB01132: VB57 - Einrichten einer Strukturdirektion
      *
      *   Rev 2.0   Mar 01 2011 14:52:12   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.007.0036
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-31)
      *   MAPSET NAME   BVB0049
      *   DATE CREATED  03/23/2011
      *   TIME CREATED  08:37:02
      *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------+--------+----+---------------------------+----------
      * AVB01132!01.03.11!2.1 ! NEU                       ! KLEEMEIER
      *         !        !    !                           !

      *  INPUT DATA FOR MAP BVB0049
         01 BVB0049I.
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
            03 ADIR1L                         PIC S9(4) COMP.
            03 ADIR1F                         PIC X.
            03 FILLER REDEFINES ADIR1F.
               05 ADIR1A                         PIC X.
            03 ADIR1I                         PIC X(3).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER REDEFINES ABTRNAMF.
               05 ABTRNAMA                       PIC X.
            03 ABTRNAMI                       PIC X(40).
            03 ASTFL                          PIC S9(4) COMP.
            03 ASTFF                          PIC X.
            03 FILLER REDEFINES ASTFF.
               05 ASTFA                          PIC X.
            03 ASTFI                          PIC X(3).
            03 EDIRL                          PIC S9(4) COMP.
            03 EDIRF                          PIC X.
            03 FILLER REDEFINES EDIRF.
               05 EDIRA                          PIC X.
            03 EDIRI                          PIC X(3).
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

      *  OUTPUT DATA FOR MAP BVB0049
         01 BVB0049O REDEFINES BVB0049I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ANAME1O                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 ASTR1O                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 AORT1O                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 ADIR1O                         PIC X(3).
            03 FILLER                         PIC X(3).
            03 ABTRNAMO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 ASTFO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 EDIRO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 AABDATO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 EAUSO                          PIC X(1).
