      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.11.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0066.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DBVB0066.CPv  $
      *
      *   Rev 2.1   Jan 07 2021 11:47:42   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.0   Nov 26 2020 16:56:48   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BVB0066
      *   DATE CREATED  12/02/2020
      *   TIME CREATED  13:44:15

      *  INPUT DATA FOR MAP BVB0066
         01 BVB0066I.
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
            03 APRFTXTL                       PIC S9(4) COMP.
            03 APRFTXTF                       PIC X.
            03 FILLER REDEFINES APRFTXTF.
               05 APRFTXTA                       PIC X.
            03 APRFTXTI                       PIC X(14).
            03 ABEFAML                        PIC S9(4) COMP.
            03 ABEFAMF                        PIC X.
            03 FILLER REDEFINES ABEFAMF.
               05 ABEFAMA                        PIC X.
            03 ABEFAMI                        PIC X(10).
            03 AISTATL                        PIC S9(4) COMP.
            03 AISTATF                        PIC X.
            03 FILLER REDEFINES AISTATF.
               05 AISTATA                        PIC X.
            03 AISTATI                        PIC X(5).
            03 ABEDNRL                        PIC S9(4) COMP.
            03 ABEDNRF                        PIC X.
            03 FILLER REDEFINES ABEDNRF.
               05 ABEDNRA                        PIC X.
            03 ABEDNRI                        PIC X(4).
            03 AIEHVONL                       PIC S9(4) COMP.
            03 AIEHVONF                       PIC X.
            03 FILLER REDEFINES AIEHVONF.
               05 AIEHVONA                       PIC X.
            03 AIEHVONI                       PIC X(5).
            03 AIEHBISL                       PIC S9(4) COMP.
            03 AIEHBISF                       PIC X.
            03 FILLER REDEFINES AIEHBISF.
               05 AIEHBISA                       PIC X.
            03 AIEHBISI                       PIC X(5).
            03 AZRANZL                        PIC S9(4) COMP.
            03 AZRANZF                        PIC X.
            03 FILLER REDEFINES AZRANZF.
               05 AZRANZA                        PIC X.
            03 AZRANZI                        PIC X(2).
            03 ASTXT1L                        PIC S9(4) COMP.
            03 ASTXT1F                        PIC X.
            03 FILLER REDEFINES ASTXT1F.
               05 ASTXT1A                        PIC X.
            03 ASTXT1I                        PIC X(15).
            03 ASEHEIZL                       PIC S9(4) COMP.
            03 ASEHEIZF                       PIC X.
            03 FILLER REDEFINES ASEHEIZF.
               05 ASEHEIZA                       PIC X.
            03 ASEHEIZI                       PIC X(10).
            03 ASEQFEIL                       PIC S9(4) COMP.
            03 ASEQFEIF                       PIC X.
            03 FILLER REDEFINES ASEQFEIF.
               05 ASEQFEIA                       PIC X.
            03 ASEQFEII                       PIC X(5).
            03 AITXT1L                        PIC S9(4) COMP.
            03 AITXT1F                        PIC X.
            03 FILLER REDEFINES AITXT1F.
               05 AITXT1A                        PIC X.
            03 AITXT1I                        PIC X(8).
            03 AIEHEIZL                       PIC S9(4) COMP.
            03 AIEHEIZF                       PIC X.
            03 FILLER REDEFINES AIEHEIZF.
               05 AIEHEIZA                       PIC X.
            03 AIEHEIZI                       PIC X(10).
            03 AIEQFEIL                       PIC S9(4) COMP.
            03 AIEQFEIF                       PIC X.
            03 FILLER REDEFINES AIEQFEIF.
               05 AIEQFEIA                       PIC X.
            03 AIEQFEII                       PIC X(5).
            03 ASTXT2L                        PIC S9(4) COMP.
            03 ASTXT2F                        PIC X.
            03 FILLER REDEFINES ASTXT2F.
               05 ASTXT2A                        PIC X.
            03 ASTXT2I                        PIC X(15).
            03 ASEHGRUL                       PIC S9(4) COMP.
            03 ASEHGRUF                       PIC X.
            03 FILLER REDEFINES ASEHGRUF.
               05 ASEHGRUA                       PIC X.
            03 ASEHGRUI                       PIC X(10).
            03 ASEQFGRL                       PIC S9(4) COMP.
            03 ASEQFGRF                       PIC X.
            03 FILLER REDEFINES ASEQFGRF.
               05 ASEQFGRA                       PIC X.
            03 ASEQFGRI                       PIC X(5).
            03 AITXT2L                        PIC S9(4) COMP.
            03 AITXT2F                        PIC X.
            03 FILLER REDEFINES AITXT2F.
               05 AITXT2A                        PIC X.
            03 AITXT2I                        PIC X(8).
            03 AIEHGRUL                       PIC S9(4) COMP.
            03 AIEHGRUF                       PIC X.
            03 FILLER REDEFINES AIEHGRUF.
               05 AIEHGRUA                       PIC X.
            03 AIEHGRUI                       PIC X(10).
            03 ABEHGRUL                       PIC S9(4) COMP.
            03 ABEHGRUF                       PIC X.
            03 FILLER REDEFINES ABEHGRUF.
               05 ABEHGRUA                       PIC X.
            03 ABEHGRUI                       PIC X(10).
            03 AIEQFGRL                       PIC S9(4) COMP.
            03 AIEQFGRF                       PIC X.
            03 FILLER REDEFINES AIEQFGRF.
               05 AIEQFGRA                       PIC X.
            03 AIEQFGRI                       PIC X(5).
            03 ASTXT3L                        PIC S9(4) COMP.
            03 ASTXT3F                        PIC X.
            03 FILLER REDEFINES ASTXT3F.
               05 ASTXT3A                        PIC X.
            03 ASTXT3I                        PIC X(15).
            03 ASEHGESL                       PIC S9(4) COMP.
            03 ASEHGESF                       PIC X.
            03 FILLER REDEFINES ASEHGESF.
               05 ASEHGESA                       PIC X.
            03 ASEHGESI                       PIC X(10).
            03 ASEQFGSL                       PIC S9(4) COMP.
            03 ASEQFGSF                       PIC X.
            03 FILLER REDEFINES ASEQFGSF.
               05 ASEQFGSA                       PIC X.
            03 ASEQFGSI                       PIC X(5).
            03 AITXT3L                        PIC S9(4) COMP.
            03 AITXT3F                        PIC X.
            03 FILLER REDEFINES AITXT3F.
               05 AITXT3A                        PIC X.
            03 AITXT3I                        PIC X(8).
            03 AIEHGESL                       PIC S9(4) COMP.
            03 AIEHGESF                       PIC X.
            03 FILLER REDEFINES AIEHGESF.
               05 AIEHGESA                       PIC X.
            03 AIEHGESI                       PIC X(10).
            03 ABEHGESL                       PIC S9(4) COMP.
            03 ABEHGESF                       PIC X.
            03 FILLER REDEFINES ABEHGESF.
               05 ABEHGESA                       PIC X.
            03 ABEHGESI                       PIC X(10).
            03 AIEQFGSL                       PIC S9(4) COMP.
            03 AIEQFGSF                       PIC X.
            03 FILLER REDEFINES AIEQFGSF.
               05 AIEQFGSA                       PIC X.
            03 AIEQFGSI                       PIC X(5).
            03 ASTXT4L                        PIC S9(4) COMP.
            03 ASTXT4F                        PIC X.
            03 FILLER REDEFINES ASTXT4F.
               05 ASTXT4A                        PIC X.
            03 ASTXT4I                        PIC X(15).
            03 AMAXEIGL                       PIC S9(4) COMP.
            03 AMAXEIGF                       PIC X.
            03 FILLER REDEFINES AMAXEIGF.
               05 AMAXEIGA                       PIC X.
            03 AMAXEIGI                       PIC X(10).
            03 AITXT4L                        PIC S9(4) COMP.
            03 AITXT4F                        PIC X.
            03 FILLER REDEFINES AITXT4F.
               05 AITXT4A                        PIC X.
            03 AITXT4I                        PIC X(8).
            03 AIEHEIGL                       PIC S9(4) COMP.
            03 AIEHEIGF                       PIC X.
            03 FILLER REDEFINES AIEHEIGF.
               05 AIEHEIGA                       PIC X.
            03 AIEHEIGI                       PIC X(10).
            03 ATXTVERL                       PIC S9(4) COMP.
            03 ATXTVERF                       PIC X.
            03 FILLER REDEFINES ATXTVERF.
               05 ATXTVERA                       PIC X.
            03 ATXTVERI                       PIC X(16).
            03 AITXT5L                        PIC S9(4) COMP.
            03 AITXT5F                        PIC X.
            03 FILLER REDEFINES AITXT5F.
               05 AITXT5A                        PIC X.
            03 AITXT5I                        PIC X(8).
            03 AEIGABZL                       PIC S9(4) COMP.
            03 AEIGABZF                       PIC X.
            03 FILLER REDEFINES AEIGABZF.
               05 AEIGABZA                       PIC X.
            03 AEIGABZI                       PIC X(10).
            03 AIEHVERL                       PIC S9(4) COMP.
            03 AIEHVERF                       PIC X.
            03 FILLER REDEFINES AIEHVERF.
               05 AIEHVERA                       PIC X.
            03 AIEHVERI                       PIC X(10).
            03 ASTXT7L                        PIC S9(4) COMP.
            03 ASTXT7F                        PIC X.
            03 FILLER REDEFINES ASTXT7F.
               05 ASTXT7A                        PIC X.
            03 ASTXT7I                        PIC X(15).
            03 ASEHNPL                        PIC S9(4) COMP.
            03 ASEHNPF                        PIC X.
            03 FILLER REDEFINES ASEHNPF.
               05 ASEHNPA                        PIC X.
            03 ASEHNPI                        PIC X(10).
            03 AITXT7L                        PIC S9(4) COMP.
            03 AITXT7F                        PIC X.
            03 FILLER REDEFINES AITXT7F.
               05 AITXT7A                        PIC X.
            03 AITXT7I                        PIC X(8).
            03 AIEHNPL                        PIC S9(4) COMP.
            03 AIEHNPF                        PIC X.
            03 FILLER REDEFINES AIEHNPF.
               05 AIEHNPA                        PIC X.
            03 AIEHNPI                        PIC X(10).
            03 ABEHNPL                        PIC S9(4) COMP.
            03 ABEHNPF                        PIC X.
            03 FILLER REDEFINES ABEHNPF.
               05 ABEHNPA                        PIC X.
            03 ABEHNPI                        PIC X(10).
            03 ASEM1TXL                       PIC S9(4) COMP.
            03 ASEM1TXF                       PIC X.
            03 FILLER REDEFINES ASEM1TXF.
               05 ASEM1TXA                       PIC X.
            03 ASEM1TXI                       PIC X(15).
            03 ASEM1SL                        PIC S9(4) COMP.
            03 ASEM1SF                        PIC X.
            03 FILLER REDEFINES ASEM1SF.
               05 ASEM1SA                        PIC X.
            03 ASEM1SI                        PIC X(1).
            03 ASEM1IL                        PIC S9(4) COMP.
            03 ASEM1IF                        PIC X.
            03 FILLER REDEFINES ASEM1IF.
               05 ASEM1IA                        PIC X.
            03 ASEM1II                        PIC X(1).
            03 AGEW1TXL                       PIC S9(4) COMP.
            03 AGEW1TXF                       PIC X.
            03 FILLER REDEFINES AGEW1TXF.
               05 AGEW1TXA                       PIC X.
            03 AGEW1TXI                       PIC X(15).
            03 AGEW1SL                        PIC S9(4) COMP.
            03 AGEW1SF                        PIC X.
            03 FILLER REDEFINES AGEW1SF.
               05 AGEW1SA                        PIC X.
            03 AGEW1SI                        PIC X(1).
            03 AGEW1IL                        PIC S9(4) COMP.
            03 AGEW1IF                        PIC X.
            03 FILLER REDEFINES AGEW1IF.
               05 AGEW1IA                        PIC X.
            03 AGEW1II                        PIC X(1).
            03 ASEM2TXL                       PIC S9(4) COMP.
            03 ASEM2TXF                       PIC X.
            03 FILLER REDEFINES ASEM2TXF.
               05 ASEM2TXA                       PIC X.
            03 ASEM2TXI                       PIC X(15).
            03 ASEM2SL                        PIC S9(4) COMP.
            03 ASEM2SF                        PIC X.
            03 FILLER REDEFINES ASEM2SF.
               05 ASEM2SA                        PIC X.
            03 ASEM2SI                        PIC X(1).
            03 ASEM2IL                        PIC S9(4) COMP.
            03 ASEM2IF                        PIC X.
            03 FILLER REDEFINES ASEM2IF.
               05 ASEM2IA                        PIC X.
            03 ASEM2II                        PIC X(1).
            03 AGEW2TXL                       PIC S9(4) COMP.
            03 AGEW2TXF                       PIC X.
            03 FILLER REDEFINES AGEW2TXF.
               05 AGEW2TXA                       PIC X.
            03 AGEW2TXI                       PIC X(15).
            03 AGEW2SL                        PIC S9(4) COMP.
            03 AGEW2SF                        PIC X.
            03 FILLER REDEFINES AGEW2SF.
               05 AGEW2SA                        PIC X.
            03 AGEW2SI                        PIC X(1).
            03 AGEW2IL                        PIC S9(4) COMP.
            03 AGEW2IF                        PIC X.
            03 FILLER REDEFINES AGEW2IF.
               05 AGEW2IA                        PIC X.
            03 AGEW2II                        PIC X(1).
            03 ASEM3TXL                       PIC S9(4) COMP.
            03 ASEM3TXF                       PIC X.
            03 FILLER REDEFINES ASEM3TXF.
               05 ASEM3TXA                       PIC X.
            03 ASEM3TXI                       PIC X(15).
            03 ASEM3SL                        PIC S9(4) COMP.
            03 ASEM3SF                        PIC X.
            03 FILLER REDEFINES ASEM3SF.
               05 ASEM3SA                        PIC X.
            03 ASEM3SI                        PIC X(1).
            03 ASEM3IL                        PIC S9(4) COMP.
            03 ASEM3IF                        PIC X.
            03 FILLER REDEFINES ASEM3IF.
               05 ASEM3IA                        PIC X.
            03 ASEM3II                        PIC X(1).
            03 AGEW3TXL                       PIC S9(4) COMP.
            03 AGEW3TXF                       PIC X.
            03 FILLER REDEFINES AGEW3TXF.
               05 AGEW3TXA                       PIC X.
            03 AGEW3TXI                       PIC X(15).
            03 AGEW3SL                        PIC S9(4) COMP.
            03 AGEW3SF                        PIC X.
            03 FILLER REDEFINES AGEW3SF.
               05 AGEW3SA                        PIC X.
            03 AGEW3SI                        PIC X(1).
            03 AGEW3IL                        PIC S9(4) COMP.
            03 AGEW3IF                        PIC X.
            03 FILLER REDEFINES AGEW3IF.
               05 AGEW3IA                        PIC X.
            03 AGEW3II                        PIC X(1).
            03 ASEM4TXL                       PIC S9(4) COMP.
            03 ASEM4TXF                       PIC X.
            03 FILLER REDEFINES ASEM4TXF.
               05 ASEM4TXA                       PIC X.
            03 ASEM4TXI                       PIC X(15).
            03 ASEM4SL                        PIC S9(4) COMP.
            03 ASEM4SF                        PIC X.
            03 FILLER REDEFINES ASEM4SF.
               05 ASEM4SA                        PIC X.
            03 ASEM4SI                        PIC X(1).
            03 ASEM4IL                        PIC S9(4) COMP.
            03 ASEM4IF                        PIC X.
            03 FILLER REDEFINES ASEM4IF.
               05 ASEM4IA                        PIC X.
            03 ASEM4II                        PIC X(1).
            03 AGEW4TXL                       PIC S9(4) COMP.
            03 AGEW4TXF                       PIC X.
            03 FILLER REDEFINES AGEW4TXF.
               05 AGEW4TXA                       PIC X.
            03 AGEW4TXI                       PIC X(15).
            03 AGEW4SL                        PIC S9(4) COMP.
            03 AGEW4SF                        PIC X.
            03 FILLER REDEFINES AGEW4SF.
               05 AGEW4SA                        PIC X.
            03 AGEW4SI                        PIC X(1).
            03 AGEW4IL                        PIC S9(4) COMP.
            03 AGEW4IF                        PIC X.
            03 FILLER REDEFINES AGEW4IF.
               05 AGEW4IA                        PIC X.
            03 AGEW4II                        PIC X(1).

      *  OUTPUT DATA FOR MAP BVB0066
         01 BVB0066O REDEFINES BVB0066I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 APRFTXTO                       PIC X(14).
            03 FILLER                         PIC X(3).
            03 ABEFAMO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AISTATO                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 ABEDNRO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AIEHVONO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 AIEHBISO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 AZRANZO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 ASTXT1O                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEHEIZO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ASEQFEIO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 AITXT1O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 AIEHEIZO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AIEQFEIO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 ASTXT2O                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEHGRUO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ASEQFGRO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 AITXT2O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 AIEHGRUO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ABEHGRUO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AIEQFGRO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 ASTXT3O                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEHGESO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ASEQFGSO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 AITXT3O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 AIEHGESO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ABEHGESO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AIEQFGSO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 ASTXT4O                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 AMAXEIGO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AITXT4O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 AIEHEIGO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ATXTVERO                       PIC X(16).
            03 FILLER                         PIC X(3).
            03 AITXT5O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 AEIGABZO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AIEHVERO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ASTXT7O                        PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEHNPO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 AITXT7O                        PIC X(8).
            03 FILLER                         PIC X(3).
            03 AIEHNPO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ABEHNPO                        PIC X(10).
            03 FILLER                         PIC X(3).
            03 ASEM1TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEM1SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEM1IO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW1TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 AGEW1SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW1IO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEM2TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEM2SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEM2IO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW2TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 AGEW2SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW2IO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEM3TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEM3SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEM3IO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW3TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 AGEW3SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW3IO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEM4TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 ASEM4SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ASEM4IO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW4TXO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 AGEW4SO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AGEW4IO                        PIC X(1).


