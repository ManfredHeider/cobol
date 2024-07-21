      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBVB0002.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DBVB0002.CPv  $
      *
      *   Rev 2.2   Aug 09 2002 11:03:28   t13ae27
      *EMAIL/URL-Einführung
      *
      *   Rev 2.1   13 Jul 2001 12:29:22   t13ae34
      *AVB00551 - VB-Portal Pin-Nummernvergabe
      *über Dialog VB42
      *
      *   Rev 2.0   May 08 2000 08:20:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DBVB0002                                     *
      *    --------------------------------------------------------  *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    JULI 2001                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00551!02.07.01!2.1 ! NEU                       ! HEIDER   *
      * AVB00611!25.06.02!2.2 ! anzeigen EMAIL und URL    ! P. Duerr *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  INPUT DATA FOR MAP BVB0041
         01 BVB0041I.
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
250602      03 EMAILL                         PIC S9(4) COMP.
250602      03 EMAILF                         PIC X.
250602      03 FILLER                         PIC X(1).
250602      03 EMAILI                         PIC X(70).
250602      03 URLTXTL                        PIC S9(4) COMP.
250602      03 URLTXTF                        PIC X.
250602      03 FILLER                         PIC X(1).
250602      03 URLTXTI                        PIC X(6).
250602      03 URLL                           PIC S9(4) COMP.
250602      03 URLF                           PIC X.
250602      03 FILLER                         PIC X(1).
250602      03 URLI                           PIC X(70).
            03 APINL                          PIC S9(4) COMP.
            03 APINF                          PIC X.
            03 FILLER                         PIC X(1).
            03 APINI                          PIC X(10).
            03 ABDATL                         PIC S9(4) COMP.
            03 ABDATF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ABDATI                         PIC X(10).
            03 APINH1L                        PIC S9(4) COMP.
            03 APINH1F                        PIC X.
            03 FILLER                         PIC X(1).
            03 APINH1I                        PIC X(10).
            03 ABDATH1L                       PIC S9(4) COMP.
            03 ABDATH1F                       PIC X.
            03 FILLER                         PIC X(1).
            03 ABDATH1I                       PIC X(10).
            03 APINH2L                        PIC S9(4) COMP.
            03 APINH2F                        PIC X.
            03 FILLER                         PIC X(1).
            03 APINH2I                        PIC X(10).
            03 ABDATH2L                       PIC S9(4) COMP.
            03 ABDATH2F                       PIC X.
            03 FILLER                         PIC X(1).
            03 ABDATH2I                       PIC X(10).
            03 APINH3L                        PIC S9(4) COMP.
            03 APINH3F                        PIC X.
            03 FILLER                         PIC X(1).
            03 APINH3I                        PIC X(10).
            03 ABDATH3L                       PIC S9(4) COMP.
            03 ABDATH3F                       PIC X.
            03 FILLER                         PIC X(1).
            03 ABDATH3I                       PIC X(10).
            03 EAUSWL                         PIC S9(4) COMP.
            03 EAUSWF                         PIC X.
            03 FILLER                         PIC X(1).
            03 EAUSWI                         PIC X(1).

      *  OUTPUT DATA FOR MAP BVB0041
         01 BVB0041O REDEFINES BVB0041I.
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
250602      03 FILLER                         PIC X(2).
250602      03 EMAILA                         PIC X.
250602      03 EMAILH                         PIC X.
250602      03 EMAILO                         PIC X(70).
250602      03 FILLER                         PIC X(2).
250602      03 URLTXTA                        PIC X.
250602      03 URLTXTH                        PIC X.
250602      03 URLTXTO                        PIC X(6).
250602      03 FILLER                         PIC X(2).
250602      03 URLA                           PIC X.
250602      03 URLH                           PIC X.
250602      03 URLO                           PIC X(70).
            03 DFHG001O OCCURS 4.
               05 FILLER                         PIC X(2).
               05 APINA                          PIC X.
               05 APINH                          PIC X.
               05 APINO                          PIC X(10).
               05 FILLER                         PIC X(2).
               05 ABDATA                         PIC X.
               05 ABDATH                         PIC X.
               05 ABDATO                         PIC X(10).
            03 FILLER                         PIC X(2).
            03 EAUSWA                         PIC X.
            03 EAUSWH                         PIC X.
            03 EAUSWO                         PIC X(1).

