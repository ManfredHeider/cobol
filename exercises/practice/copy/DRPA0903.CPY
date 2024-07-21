      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0903.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0903.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:11:34   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.0   May 26 1999 15:29:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0903 DATENFELDER FüR DIALOG PO25         *
      *                          'PROVISIONSBESTAND PROV-STRUKTUR'   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: FOLTZ                                           *
      *    DATUM:    08.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.05.98! 01 !NEU                        !FOLTZ     *
      * APA01932!14.04.15!2.01! ERWEITERUNGEN FÜR MASKE 3 ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FüR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG PO25                      *
      *                                                              *
      ****************************************************************


      ***    LISTFELDER MASKE 1 + 2
             35 PEBD-ST-NR                       PIC S9(009)  BINARY.
             35 ST-PRZ                           PIC  X(001).
             35 PE-NAME                          PIC  X(020).

      ***    LISTFELDER MASKE 1
             35 PEBD-DIFF-PROV-STF               PIC S9(009)V99
                                                 PACKED-DECIMAL.
             35 KNR-EMP-TYP-BM                   PIC  X(010).
             35 PROV-BETR                        PIC S9(009)V99
                                                 PACKED-DECIMAL.
             35 PDPE-EINH                        PIC S9(006)V99
                                                 PACKED-DECIMAL.
             35 PDPT-MA-ABR-DAT                  PIC  X(010).

      ***    LISTFELDER MASKE 2
             35 KNR-AUSZ-WAEHRUNG-BK             PIC  X(004).
             35 PDPE-NT-PBTRG-AZWG               PIC S9(009)V99
                                                 PACKED-DECIMAL.
             35 PDPE-PROV-RCK-AZWG               PIC S9(009)V99
                                                 PACKED-DECIMAL.
             35 RCK-STZ                          PIC S9(004) BINARY.

140415***    LISTFELDER MASKE 3
             35 PDPE-KZ-UST-BER                  PIC  X(001).
             35 PDPE-UST-PBTRG-BRWG              PIC S9(009)V99
                                                 PACKED-DECIMAL.
             35 STEUERL-STATUS-DAT               PIC  X(010).


