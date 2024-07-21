      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0483.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0483.CPv  $
      *
      *   Rev 2.1   26 May 2000 14:10:58   T13AE27
      *neue Funktion DRVB0483
      *
      *   Rev 2.0   19 May 2000 09:49:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0484                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0484 (Programm ZWVB0026)*
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.DUERR                                          *
      *    DATUM:    25.05.2000                                       *
      *****************************************************************
      *    VERSION 1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00054!25.05.00!    ! NEU                       ! P.DUERR  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0483:
      *    EINGABE:                         MA-ST-NR
      *                                     OLZZ-MONAT (TT.MM.JJJJ)
      *                                     KNR-OLZ-TYP
      *    RUECKGABE:
      *                                     GANZER TUPEL VON OLZZ
      *                                     FACHLICHER RC
      ****************************************************************
      *
       01  PI-DRVB0483.
           05  PI-DRVB0483-DATEN.
               10 PI-DRVB0483-MA-ST-NR            PIC S9(009) BINARY.
               10 PI-DRVB0483-OLZZ-MONAT          PIC  X(010).
               10 PI-DRVB0483-KNR-OLZ-TYP         PIC S9(004) BINARY.
      *
       01  PO-DRVB0483.
           05  PO-DRVB0483-RC                  PIC  X(003).
               88 PO-DRVB0483-RC-OK            VALUE 'OK '.
               88 PO-DRVB0483-RC-NOT-FOUND     VALUE 'NFD'.
           05  PO-DRVB0483-DATEN.
               10 PO-DRVB0483-MA-ST-NR            PIC S9(009) BINARY.
               10 PO-DRVB0483-OLZZ-MONAT          PIC X(010).
               10 PO-DRVB0483-KNR-OLZ-TYP         PIC S9(004) BINARY.
      *        10 PO-DRVB0483-KNR-WAEHRUNG        PIC S9(004) BINARY.
      *        10 PO-DRVB0483-KTY-OLZ-TYP         PIC S9(004) BINARY.
      *        10 PO-DRVB0483-KTY-WAEHRUNG        PIC S9(004) BINARY.
               10 PO-DRVB0483-OLZZ-BETRAG         PIC S9(009)V99 COMP-3.
      *

