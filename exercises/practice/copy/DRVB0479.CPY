      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0479.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0479.CPv  $
      *
      *   Rev 2.1   22 May 2000 14:12:26   T13AE27
      *neue Funktion für ZWVB0026
      *
      *   Rev 2.0   Apr 03 2000 14:11:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0479                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0479                    *
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
      *    FÜR DIE FUNKTION DRVB0479:
      *    EINGABE:                         MA-ST-NR
      *                                     OLZZ-MONAT in Format MM.JJJJ
      *                                     KNR-OLZ-TYP
      *    RUECKGABE:
      *                                     GANZER TUPEL VON OLZZ
      *                                     FACHLICHER RC
      ****************************************************************
      *
       01  PI-DRVB0479.
           05  PI-DRVB0479-DATEN.
               10 PI-DRVB0479-MA-ST-NR            PIC S9(009) BINARY.
               10 PI-DRVB0479-OLZZ-MONAT          PIC  X(007).
               10 PI-DRVB0479-KNR-OLZ-TYP         PIC S9(004) BINARY.
      *
       01  PO-DRVB0479.
           05  PO-DRVB0479-RC                  PIC  X(003).
               88 PO-DRVB0479-RC-OK            VALUE 'OK '.
               88 PO-DRVB0479-RC-NOT-FOUND     VALUE 'NFD'.
           05  PO-DRVB0479-DATEN.
               10 PO-DRVB0479-MA-ST-NR            PIC S9(009) BINARY.
               10 PO-DRVB0479-OLZZ-MONAT          PIC X(007).
               10 PO-DRVB0479-KNR-OLZ-TYP         PIC S9(004) BINARY.
      *        10 PO-DRVB0479-KNR-WAEHRUNG        PIC S9(004) BINARY.
      *        10 PO-DRVB0479-KTY-OLZ-TYP         PIC S9(004) BINARY.
      *        10 PO-DRVB0479-KTY-WAEHRUNG        PIC S9(004) BINARY.
               10 PO-DRVB0479-OLZZ-BETRAG         PIC S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

