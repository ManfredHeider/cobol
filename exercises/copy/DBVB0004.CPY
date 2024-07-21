      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DBVB0004.CPv  $
      *
      *   Rev 2.1   Feb 20 2003 14:42:04   t13ex22
      *AAN00660: Erfassen eMail im VB43
      *
      *   Rev 2.0   May 08 2000 08:20:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: BVB0004                                     *
      *    --------------------------------------------------------  *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    JANUAR 2003                                     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00660!27.01.03!2.1 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *   Micro Focus BMS Screen Painter (ver MFBMW 2.5.10)
      *   MapSet Name   BVB0004
      *   Date Created  02/19/2003
      *   Time Created  09:25:02

      *  Input Data For Map BVB0004
         01 BVB0004I.
            03 FILLER                         PIC X(12).
            03 ESTL                           PIC S9(4) COMP.
            03 ESTF                           PIC X.
            03 FILLER                         PIC X(1).
            03 ESTI                           PIC X(7).
            03 ESTPL                          PIC S9(4) COMP.
            03 ESTPF                          PIC X.
            03 FILLER                         PIC X(1).
            03 ESTPI                          PIC X(1).
            03 AINFOZLL                       PIC S9(4) COMP.
            03 AINFOZLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AINFOZLI                       PIC X(65).
            03 ANAMEL                         PIC S9(4) COMP.
            03 ANAMEF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ANAMEI                         PIC X(20).
            03 AVORNAML                       PIC S9(4) COMP.
            03 AVORNAMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVORNAMI                       PIC X(15).
            03 ABTRNAML                       PIC S9(4) COMP.
            03 ABTRNAMF                       PIC X.
            03 FILLER                         PIC X(1).
            03 ABTRNAMI                       PIC X(16).
            03 EMAILL                         PIC S9(4) COMP.
            03 EMAILF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EMAILI                         PIC X(70).
            03 URLTXTL                        PIC S9(4) COMP.
            03 URLTXTF                        PIC X.
            03 FILLER                         PIC X(1).
            03 URLTXTI                        PIC X(6).
            03 URLL                           PIC S9(4) COMP.
            03 URLF                           PIC X.
            03 FILLER                         PIC X(1).
            03 URLI                           PIC X(70).
            03 ABDATL                         PIC S9(4) COMP.
            03 ABDATF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ABDATI                         PIC X(10).
            03 EEMAILL                        PIC S9(4) COMP.
            03 EEMAILF                        PIC X.
            03 FILLER                         PIC X(1).
            03 EEMAILI                        PIC X(70).
            03 AEMAILHD                       OCCURS 4 TIMES.
               05 AEMAILHL                       PIC S9(4) COMP.
               05 AEMAILHF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AEMAILHI                       PIC X(51).
            03 AABDATHD                       OCCURS 4 TIMES.
               05 AABDATHL                       PIC S9(4) COMP.
               05 AABDATHF                       PIC X.
               05 FILLER                         PIC X(1).
               05 AABDATHI                       PIC X(10).
            03 AUSWAHLL                       PIC S9(4) COMP.
            03 AUSWAHLF                       PIC X.
            03 FILLER                         PIC X(1).
            03 AUSWAHLI                       PIC X(8).
            03 EAUSWL                         PIC S9(4) COMP.
            03 EAUSWF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EAUSWI                         PIC X(1).
            03 NEUVERGL                       PIC S9(4) COMP.
            03 NEUVERGF                       PIC X.
            03 FILLER                         PIC X(1).
            03 NEUVERGI                       PIC X(18).

      *  Output Data For Map BVB0004
         01 BVB0004O REDEFINES BVB0004I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 ESTA                           PIC X.
            03 ESTH                           PIC X.
            03 ESTO                           PIC X(7).
            03 FILLER                         PIC X(2).
            03 ESTPA                          PIC X.
            03 ESTPH                          PIC X.
            03 ESTPO                          PIC X(1).
            03 FILLER                         PIC X(2).
            03 AINFOZLA                       PIC X.
            03 AINFOZLH                       PIC X.
            03 AINFOZLO                       PIC X(65).
            03 FILLER                         PIC X(2).
            03 ANAMEA                         PIC X.
            03 ANAMEH                         PIC X.
            03 ANAMEO                         PIC X(20).
            03 FILLER                         PIC X(2).
            03 AVORNAMA                       PIC X.
            03 AVORNAMH                       PIC X.
            03 AVORNAMO                       PIC X(15).
            03 FILLER                         PIC X(2).
            03 ABTRNAMA                       PIC X.
            03 ABTRNAMH                       PIC X.
            03 ABTRNAMO                       PIC X(16).
            03 FILLER                         PIC X(2).
            03 EMAILA                         PIC X.
            03 EMAILH                         PIC X.
            03 EMAILO                         PIC X(70).
            03 FILLER                         PIC X(2).
            03 URLTXTA                        PIC X.
            03 URLTXTH                        PIC X.
            03 URLTXTO                        PIC X(6).
            03 FILLER                         PIC X(2).
            03 URLA                           PIC X.
            03 URLH                           PIC X.
            03 URLO                           PIC X(70).
            03 FILLER                         PIC X(2).
            03 ABDATA                         PIC X.
            03 ABDATH                         PIC X.
            03 ABDATO                         PIC X(10).
            03 FILLER                         PIC X(2).
            03 EEMAILA                        PIC X.
            03 EEMAILH                        PIC X.
            03 EEMAILO                        PIC X(70).
            03 DFHMS1 OCCURS 4.
               05 FILLER                         PIC X(2).
               05 AEMAILHA                       PIC X.
               05 AEMAILHH                       PIC X.
               05 AEMAILHO                       PIC X(51).
            03 DFHMS2 OCCURS 4.
               05 FILLER                         PIC X(2).
               05 AABDATHA                       PIC X.
               05 AABDATHH                       PIC X.
               05 AABDATHO                       PIC X(10).
            03 FILLER                         PIC X(2).
            03 AUSWAHLA                       PIC X.
            03 AUSWAHLH                       PIC X.
            03 AUSWAHLO                       PIC X(8).
            03 FILLER                         PIC X(2).
            03 EAUSWA                         PIC X.
            03 EAUSWH                         PIC X.
            03 EAUSWO                         PIC X(1).
            03 FILLER                         PIC X(2).
            03 NEUVERGA                       PIC X.
            03 NEUVERGH                       PIC X.
            03 NEUVERGO                       PIC X(18).