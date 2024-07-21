      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME     *
      * -----------+--------+-----------------------------+----------*
200122* VLA-4046   !20.01.22! MONATLICHE BASISSTUFE UND   !          *
200122*            !        ! HINWEIS EQF-ABZUG HINZU     ! KLEEMEIER*
      *            !        !                             !          *
      *                                                              *
      ****************************************************************
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BSL0019
      *   DATE CREATED  01/21/2022
      *   TIME CREATED  09:11:27

      *  INPUT DATA FOR MAP BSL0019
         01 BSL0019I.
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
            03 AINFOZLL                       PIC S9(4) COMP.
            03 AINFOZLF                       PIC X.
            03 FILLER REDEFINES AINFOZLF.
               05 AINFOZLA                       PIC X.
            03 AINFOZLI                       PIC X(65).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER REDEFINES ANAMEF.
               05 ANAMEA                         PIC X.
            03 ANAMEI                         PIC X(20).
            03 AVORNAML                       PIC S9(4) COMP.
            03 AVORNAMF                       PIC X.
            03 FILLER REDEFINES AVORNAMF.
               05 AVORNAMA                       PIC X.
            03 AVORNAMI                       PIC X(15).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER REDEFINES ABTRNAMF.
               05 ABTRNAMA                       PIC X.
            03 ABTRNAMI                       PIC X(16).
            03 EARTL                          PIC S9(4) COMP.
            03 EARTF                          PIC X.
            03 FILLER REDEFINES EARTF.
               05 EARTA                          PIC X.
            03 EARTI                          PIC X(1).
            03 APERDATL                       PIC S9(4) COMP.
            03 APERDATF                       PIC X.
            03 FILLER REDEFINES APERDATF.
               05 APERDATA                       PIC X.
            03 APERDATI                       PIC X(10).
            03 ATEXT1L                        PIC S9(4) COMP.
            03 ATEXT1F                        PIC X.
            03 FILLER REDEFINES ATEXT1F.
               05 ATEXT1A                        PIC X.
            03 ATEXT1I                        PIC X(25).
            03 EFSTFEL                        PIC S9(4) COMP.
            03 EFSTFEF                        PIC X.
            03 FILLER REDEFINES EFSTFEF.
               05 EFSTFEA                        PIC X.
            03 EFSTFEI                        PIC X(1).
            03 AFAPDATL                       PIC S9(4) COMP.
            03 AFAPDATF                       PIC X.
            03 FILLER REDEFINES AFAPDATF.
               05 AFAPDATA                       PIC X.
            03 AFAPDATI                       PIC X(10).
            03 AFSTFL                         PIC S9(4) COMP.
            03 AFSTFF                         PIC X.
            03 FILLER REDEFINES AFSTFF.
               05 AFSTFA                         PIC X.
            03 AFSTFI                         PIC X(1).
            03 ADSTFL                         PIC S9(4) COMP.
            03 ADSTFF                         PIC X.
            03 FILLER REDEFINES ADSTFF.
               05 ADSTFA                         PIC X.
            03 ADSTFI                         PIC X(1).
            03 AABSTFL                        PIC S9(4) COMP.
            03 AABSTFF                        PIC X.
            03 FILLER REDEFINES AABSTFF.
               05 AABSTFA                        PIC X.
            03 AABSTFI                        PIC X(3).
            03 AEHTXT1L                       PIC S9(4) COMP.
            03 AEHTXT1F                       PIC X.
            03 FILLER REDEFINES AEHTXT1F.
               05 AEHTXT1A                       PIC X.
            03 AEHTXT1I                       PIC X(8).
            03 AGSEH3ML                       PIC S9(4) COMP.
            03 AGSEH3MF                       PIC X.
            03 FILLER REDEFINES AGSEH3MF.
               05 AGSEH3MA                       PIC X.
            03 AGSEH3MI                       PIC X(12).
            03 AEHTXT2L                       PIC S9(4) COMP.
            03 AEHTXT2F                       PIC X.
            03 FILLER REDEFINES AEHTXT2F.
               05 AEHTXT2A                       PIC X.
            03 AEHTXT2I                       PIC X(8).
            03 AGSEHKOL                       PIC S9(4) COMP.
            03 AGSEHKOF                       PIC X.
            03 FILLER REDEFINES AGSEHKOF.
               05 AGSEHKOA                       PIC X.
            03 AGSEHKOI                       PIC X(12).
            03 ASTERN1L                       PIC S9(4) COMP.
            03 ASTERN1F                       PIC X.
            03 FILLER REDEFINES ASTERN1F.
               05 ASTERN1A                       PIC X.
            03 ASTERN1I                       PIC X(1).
            03 AEHGSL                         PIC S9(4) COMP.
            03 AEHGSF                         PIC X.
            03 FILLER REDEFINES AEHGSF.
               05 AEHGSA                         PIC X.
            03 AEHGSI                         PIC X(6).
            03 AGSEHEGL                       PIC S9(4) COMP.
            03 AGSEHEGF                       PIC X.
            03 FILLER REDEFINES AGSEHEGF.
               05 AGSEHEGA                       PIC X.
            03 AGSEHEGI                       PIC X(12).
            03 AGSEQFL                        PIC S9(4) COMP.
            03 AGSEQFF                        PIC X.
            03 FILLER REDEFINES AGSEQFF.
               05 AGSEQFA                        PIC X.
            03 AGSEQFI                        PIC X(5).
            03 ASTERN2L                       PIC S9(4) COMP.
            03 ASTERN2F                       PIC X.
            03 FILLER REDEFINES ASTERN2F.
               05 ASTERN2A                       PIC X.
            03 ASTERN2I                       PIC X(1).
            03 ATXTGSL                        PIC S9(4) COMP.
            03 ATXTGSF                        PIC X.
            03 FILLER REDEFINES ATXTGSF.
               05 ATXTGSA                        PIC X.
            03 ATXTGSI                        PIC X(4).
            03 AGSEHGSL                       PIC S9(4) COMP.
            03 AGSEHGSF                       PIC X.
            03 FILLER REDEFINES AGSEHGSF.
               05 AGSEHGSA                       PIC X.
            03 AGSEHGSI                       PIC X(12).
            03 AGSERFL                        PIC S9(4) COMP.
            03 AGSERFF                        PIC X.
            03 FILLER REDEFINES AGSERFF.
               05 AGSERFA                        PIC X.
            03 AGSERFI                        PIC X(1).
            03 AEHMAXL                        PIC S9(4) COMP.
            03 AEHMAXF                        PIC X.
            03 FILLER REDEFINES AEHMAXF.
               05 AEHMAXA                        PIC X.
            03 AEHMAXI                        PIC X(6).
            03 AABPSTL                        PIC S9(4) COMP.
            03 AABPSTF                        PIC X.
            03 FILLER REDEFINES AABPSTF.
               05 AABPSTA                        PIC X.
            03 AABPSTI                        PIC X(1).
            03 APROZ1L                        PIC S9(4) COMP.
            03 APROZ1F                        PIC X.
            03 FILLER REDEFINES APROZ1F.
               05 APROZ1A                        PIC X.
            03 APROZ1I                        PIC X(3).
            03 AEGEHNTL                       PIC S9(4) COMP.
            03 AEGEHNTF                       PIC X.
            03 FILLER REDEFINES AEGEHNTF.
               05 AEGEHNTA                       PIC X.
            03 AEGEHNTI                       PIC X(12).
            03 ASTERN3L                       PIC S9(4) COMP.
            03 ASTERN3F                       PIC X.
            03 FILLER REDEFINES ASTERN3F.
               05 ASTERN3A                       PIC X.
            03 ASTERN3I                       PIC X(1).
            03 AEHEGL                         PIC S9(4) COMP.
            03 AEHEGF                         PIC X.
            03 FILLER REDEFINES AEHEGF.
               05 AEHEGA                         PIC X.
            03 AEHEGI                         PIC X(6).
            03 APROZ2L                        PIC S9(4) COMP.
            03 APROZ2F                        PIC X.
            03 FILLER REDEFINES APROZ2F.
               05 APROZ2A                        PIC X.
            03 APROZ2I                        PIC X(3).
            03 AEGEQFL                        PIC S9(4) COMP.
            03 AEGEQFF                        PIC X.
            03 FILLER REDEFINES AEGEQFF.
               05 AEGEQFA                        PIC X.
            03 AEGEQFI                        PIC X(5).
            03 ASTERN4L                       PIC S9(4) COMP.
            03 ASTERN4F                       PIC X.
            03 FILLER REDEFINES ASTERN4F.
               05 ASTERN4A                       PIC X.
            03 ASTERN4I                       PIC X(1).
            03 ATXTEGL                        PIC S9(4) COMP.
            03 ATXTEGF                        PIC X.
            03 FILLER REDEFINES ATXTEGF.
               05 ATXTEGA                        PIC X.
            03 ATXTEGI                        PIC X(4).
            03 AEQFGSL                        PIC S9(4) COMP.
            03 AEQFGSF                        PIC X.
            03 FILLER REDEFINES AEQFGSF.
               05 AEQFGSA                        PIC X.
            03 AEQFGSI                        PIC X(4).
            03 AEGERFL                        PIC S9(4) COMP.
            03 AEGERFF                        PIC X.
            03 FILLER REDEFINES AEGERFF.
               05 AEGERFA                        PIC X.
            03 AEGERFI                        PIC X(1).
            03 AEQFEGL                        PIC S9(4) COMP.
            03 AEQFEGF                        PIC X.
            03 FILLER REDEFINES AEQFEGF.
               05 AEQFEGA                        PIC X.
            03 AEQFEGI                        PIC X(4).

      *  OUTPUT DATA FOR MAP BSL0019
         01 BSL0019O REDEFINES BSL0019I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 ESTNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 ESTPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AINFOZLO                       PIC X(65).
            03 FILLER                         PIC X(3).
            03 ANAMEO                         PIC X(20).
            03 FILLER                         PIC X(3).
            03 AVORNAMO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 ABTRNAMO                       PIC X(16).
            03 FILLER                         PIC X(3).
            03 EARTO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 APERDATO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 ATEXT1O                        PIC X(25).
            03 FILLER                         PIC X(3).
            03 EFSTFEO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AFAPDATO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 AFSTFO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADSTFO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 AABSTFO                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 AEHTXT1O                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 AGSEH3MO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 AEHTXT2O                       PIC X(8).
            03 FILLER                         PIC X(3).
            03 AGSEHKOO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 ASTERN1O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AEHGSO                         PIC X(6).
            03 FILLER                         PIC X(3).
            03 AGSEHEGO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 AGSEQFO                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 ASTERN2O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ATXTGSO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AGSEHGSO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 AGSERFO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AEHMAXO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 AABPSTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 APROZ1O                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 AEGEHNTO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 ASTERN3O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 AEHEGO                         PIC X(6).
            03 FILLER                         PIC X(3).
            03 APROZ2O                        PIC X(3).
            03 FILLER                         PIC X(3).
            03 AEGEQFO                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 ASTERN4O                       PIC X(1).
            03 FILLER                         PIC X(3).
            03 ATXTEGO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AEQFGSO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 AEGERFO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AEQFEGO                        PIC X(4).
