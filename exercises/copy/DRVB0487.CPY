      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0487.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0487.CPv  $
      *
      *   Rev 2.1   16 Jun 2000 15:00:54   T13AE27
      *neue Funktion
      *
      *   Rev 2.0   19 May 2000 09:49:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0487                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0487                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.DUERR                                          *
      *    DATUM:    15.06.2000                                       *
      *****************************************************************
      *    VERSION 1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00061!15.06.00!    ! NEU                       ! P.DUERR  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0487:
      *    EINGABE:                         ST-NR
      *                                     SLM-DAT-VON (TT.MM.JJJJ)
      *                                     KNR-SLM-TYP
      *    RUECKGABE:
      *                                     GANZER TUPEL VON SLM
      *                                     FACHLICHER RC
      ****************************************************************
      *
       01  PI-DRVB0487.
           05  PI-DRVB0487-DATEN.
               10 PI-DRVB0487-ST-NR            PIC S9(009) BINARY.
               10 PI-DRVB0487-SLM-DAT-VON      PIC  X(010).
               10 PI-DRVB0487-KNR-SLM-TYP      PIC S9(004) BINARY.
      *
       01  PO-DRVB0487.
           05  PO-DRVB0487-RC                  PIC  X(003).
               88 PO-DRVB0487-RC-OK            VALUE 'OK '.
               88 PO-DRVB0487-RC-NOT-FOUND     VALUE 'NFD'.
           05  PO-DRVB0487-DATEN.
               10 PO-DRVB0487-ST-NR             PIC S9(009) BINARY.
               10 PO-DRVB0487-SLM-DAT-VON       PIC X(10).
               10 PO-DRVB0487-KNR-SLM-TYP       PIC S9(004) BINARY.
      *        10 PO-DRVB0487-KNR-WAEHRUNG      PIC S9(004) BINARY.
      *        10 PO-DRVB0487-SLM-LAST-UPDATE   PIC X(26).
      *        10 PO-DRVB0487-KTY-SLM-TYP       PIC S9(004) BINARY.
      *        10 PO-DRVB0487-KTY-WAEHRUNG      PIC S9(004) BINARY.
               10 PO-DRVB0487-SLM-AUSZAHLUNG    PIC S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

