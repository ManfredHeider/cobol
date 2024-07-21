      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1117.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1117.CPv  $
      *
      *   Rev 2.1   05 Oct 2000 09:27:30   T13EX49
      *AKI00143 - Liste Neuverträge ohne gültige
      *VB-Nummer
      *
      *   Rev 2.0   07 Jun 2000 17:08:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1117                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS PROGRAMM PMKIxxxx                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                      *
      *    DATUM:    29.08.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00143!29.08.00! 01 ! NEUERSTELLUNG             !Peter Pohl*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE FÜR ALLE
      *    NEUEN VERTRÄGE, DIE OHNE GÜLTIGE VB-NUMMER GELIEFERT
      *    WERDEN.
      *
      *
      ****************************************************************
      *
       01  DRKI1117.
      *
           05 V-GESELLSCHAFT          PIC  9(002).
           05 V-VNR                   PIC  9(015).
           05 V-VBNR-GELIEFERT        PIC  X(007).
           05 V-PIF-FELD              PIC  X(020).
           05 FILLER                  PIC  X(084).

      *
      ******* ENDE COPY-MEMBER DRKI1117 *****************************
