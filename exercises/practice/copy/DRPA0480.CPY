      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0480.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0480.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:56:28   T13EX25
      *APA00042, Erstellung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:42:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0480  Satzaufbau des Workflows PEST      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    26.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !26.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      * APA00012!28.07.98!    ! NEU: KNR-AKZ-PEST         ! PARENT   *
      *         !        !    !                           !          *
      * apa00012!13.08.98! 1.2! neu: Strukturerfassungs   ! Parent   *
      *         !        !    ! und -änderungsdatum & VM  !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       20  PEST.
           25 PEST-ID            PIC S9(015) PACKED-DECIMAL.
           25 AVERM              PIC S9(009) BINARY.
           25 KD-BETR            PIC S9(009) BINARY.
130898     25 PEST-VM            PIC S9(009) BINARY.
           25 VM-GESCHAEFT-KZ    PIC  X(001).
           25 KNR-PEST-ENTSTEH   PIC S9(004) BINARY.
           25 KTY-PEST-ENTSTEH   PIC S9(004) BINARY.
           25 AKZ-EVS-ANPASSUNG  PIC  X(001).
           25 AKZ-N-D-PROV       PIC  X(001).
           25 AKZ-STRK-BEREIN    PIC  X(001).
           25 AKZ-KDB-ANPASSEN   PIC  X(001).
130898     25 STRUK-AEN-DAT      PIC  X(010).
130898     25 STRUK-DAT          PIC  X(010).
           25 PEST-VORGAENGER    PIC S9(015) PACKED-DECIMAL.
           25 VSTATUS            PIC  X(004).
           25 VSTATUS-UNR        PIC S9(004) BINARY.
           25 LAST-UPDATE        PIC  X(026).
           25 KENNUNG            PIC  X(008).
      * Feld ist nicht zu speichern
280798     25 KNR-AKZ-PEST       PIC S9(004) BINARY.
           COPY DRPA0481.
