      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0480.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0480.CPv  $
      *
      *   Rev 2.1   22 May 2000 14:13:52   T13AE27
      *neue Funktion für ZWVB0064
      *
      *   Rev 2.0   Apr 03 2000 14:11:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0480                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0480                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.DUERR                                          *
      *    DATUM:    08.05.2000                                       *
      *****************************************************************
      *    VERSION 1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00053!08.05.00!    ! NEU                       ! P.DUERR  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0480:
      *    EINGABE:                         ST-NR
      *                                     SLM-DAT-VON in Form MM.JJJJ
      *                                     KNR-SLM-TYP
      *    RUECKGABE:
      *                                     GANZER TUPEL VON SLM
      *                                     FACHLICHER RC
      ****************************************************************
      *
       01  PI-DRVB0480.
           05  PI-DRVB0480-DATEN.
               10 PI-DRVB0480-ST-NR            PIC S9(009) BINARY.
               10 PI-DRVB0480-SLM-DAT-VON      PIC  X(007).
               10 PI-DRVB0480-KNR-SLM-TYP      PIC S9(004) BINARY.
      *
       01  PO-DRVB0480.
           05  PO-DRVB0480-RC                  PIC  X(003).
               88 PO-DRVB0480-RC-OK            VALUE 'OK '.
               88 PO-DRVB0480-RC-NOT-FOUND     VALUE 'NFD'.
           05  PO-DRVB0480-DATEN.
               10 PO-DRVB0480-ST-NR             PIC S9(009) BINARY.
               10 PO-DRVB0480-SLM-DAT-VON       PIC X(10).
               10 PO-DRVB0480-KNR-SLM-TYP       PIC S9(004) BINARY.
      *        10 PO-DRVB0480-KNR-WAEHRUNG      PIC S9(004) BINARY.
      *        10 PO-DRVB0480-SLM-LAST-UPDATE   PIC X(26).
      *        10 PO-DRVB0480-KTY-SLM-TYP       PIC S9(004) BINARY.
      *        10 PO-DRVB0480-KTY-WAEHRUNG      PIC S9(004) BINARY.
               10 PO-DRVB0480-SLM-AUSZAHLUNG    PIC S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

