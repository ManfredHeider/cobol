      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBSL0012.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DBSL0012.CPv  $
      *
      *   Rev 2.2   Jun 01 2017 15:06:56   t13ae13
      *ASL00567: LUCY2: SL10 (QED-Provision) anpassen (SWL)
      *
      *   Rev 2.1   Mar 18 2011 15:46:54   t13ex13
      *AVB01103: Transaktion SL10: QED-Provision
      *
      *   Rev 2.0   Feb 18 2011 15:05:44   t13ex13
      *Archiverstellung
      *
      *   Mainframe Express 3.1  6.011.0034
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-53)
      *   MapSet Name   BSL0012
      *   Date Created  05/16/2017
      *   Time Created  17:16:07

      *  Input Data For Map BSL0012
         01 BSL0012I.
            03 FILLER                         PIC X(12).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).
            03 EVBNRDLL                       PIC S9(4) COMP.
            03 EVBNRDLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 EVBNRDLI                       PIC X(7).
            03 EDLPRZL                        PIC S9(4) COMP.
            03 EDLPRZF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EDLPRZI                        PIC X(1).
            03 ADIRNRL                        PIC S9(4) COMP.
            03 ADIRNRF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ADIRNRI                        PIC X(3).
            03 AQUARTL                        PIC S9(4) COMP.
            03 AQUARTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 AQUARTI                        PIC X(17).
            03 ASUMDLL                        PIC S9(4) COMP.
            03 ASUMDLF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ASUMDLI                        PIC X(11).
            03 ADLNAMEL                       PIC S9(4) COMP.
            03 ADLNAMEF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ADLNAMEI                       PIC X(26).
            03 EEDKTOL                        PIC S9(4) COMP.
            03 EEDKTOF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EEDKTOI                        PIC X(7).
            03 AEDKNAML                       PIC S9(4) COMP.
            03 AEDKNAMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AEDKNAMI                       PIC X(20).
            03 ASTSTATL                       PIC S9(4) COMP.
            03 ASTSTATF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASTSTATI                       PIC X(2).
            03 DFHG001I                       OCCURS 5 TIMES.
               05 ABETRAGL                       PIC S9(4) COMP.
               05 ABETRAGF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ABETRAGI                       PIC X(10).
               05 EBETRUL                        PIC S9(4) COMP.
               05 EBETRUF                        PIC X.
               05 FILLER                         PIC X(1).
               05 EBETRUI                        PIC X(10).
               05 ASTATUSL                       PIC S9(4) COMP.
               05 ASTATUSF                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASTATUSI                       PIC X(5).
               05 ATEXTL                         PIC S9(4) COMP.
               05 ATEXTF                         PIC X.
               05 FILLER                         PIC X(1).
               05 ATEXTI                         PIC X(30).
            03 ATOTALL                        PIC S9(4) COMP.
            03 ATOTALF                        PIC X.
            03 FILLER                         PIC X(1).
            03 ATOTALI                        PIC X(11).
            03 ATOTALUL                       PIC S9(4) COMP.
            03 ATOTALUF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ATOTALUI                       PIC X(11).

      *  Output Data For Map BSL0012
         01 BSL0012O REDEFINES BSL0012I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 AFCANZA                        PIC X.
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).
            03 FILLER                         PIC X(2).
            03 EVBNRDLA                       PIC X.
            03 EVBNRDLH                       PIC X.
            03 EVBNRDLO                       PIC X(7).
            03 FILLER                         PIC X(2).
            03 EDLPRZA                        PIC X.
            03 EDLPRZH                        PIC X.
            03 EDLPRZO                        PIC X(1).
            03 FILLER                         PIC X(2).
            03 ADIRNRA                        PIC X.
            03 ADIRNRH                        PIC X.
            03 ADIRNRO                        PIC X(3).
            03 FILLER                         PIC X(2).
            03 AQUARTA                        PIC X.
            03 AQUARTH                        PIC X.
            03 AQUARTO                        PIC X(17).
            03 FILLER                         PIC X(2).
            03 ASUMDLA                        PIC X.
            03 ASUMDLH                        PIC X.
            03 ASUMDLO                        PIC X(11).
            03 FILLER                         PIC X(2).
            03 ADLNAMEA                       PIC X.
            03 ADLNAMEH                       PIC X.
            03 ADLNAMEO                       PIC X(26).
            03 FILLER                         PIC X(2).
            03 EEDKTOA                        PIC X.
            03 EEDKTOH                        PIC X.
            03 EEDKTOO                        PIC X(7).
            03 FILLER                         PIC X(2).
            03 AEDKNAMA                       PIC X.
            03 AEDKNAMH                       PIC X.
            03 AEDKNAMO                       PIC X(20).
            03 FILLER                         PIC X(2).
            03 ASTSTATA                       PIC X.
            03 ASTSTATH                       PIC X.
            03 ASTSTATO                       PIC X(2).
            03 DFHG001O OCCURS 5.
               05 FILLER                         PIC X(2).
               05 ABETRAGA                       PIC X.
               05 ABETRAGH                       PIC X.
               05 ABETRAGO                       PIC X(10).
               05 FILLER                         PIC X(2).
               05 EBETRUA                        PIC X.
               05 EBETRUH                        PIC X.
               05 EBETRUO                        PIC X(10).
               05 FILLER                         PIC X(2).
               05 ASTATUSA                       PIC X.
               05 ASTATUSH                       PIC X.
               05 ASTATUSO                       PIC X(5).
               05 FILLER                         PIC X(2).
               05 ATEXTA                         PIC X.
               05 ATEXTH                         PIC X.
               05 ATEXTO                         PIC X(30).
            03 FILLER                         PIC X(2).
            03 ATOTALA                        PIC X.
            03 ATOTALH                        PIC X.
            03 ATOTALO                        PIC X(11).
            03 FILLER                         PIC X(2).
            03 ATOTALUA                       PIC X.
            03 ATOTALUH                       PIC X.
            03 ATOTALUO                       PIC X(11).


