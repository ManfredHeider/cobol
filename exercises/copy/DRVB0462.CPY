      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0462.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0462.CPv  $
      *
      *   Rev 2.2   02 Aug 2000 10:18:36   T13AE28
      *ASL00063: Entfernen 36-Monatsfrist
      *
      *   Rev 2.1   Apr 18 2000 12:42:58   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.0   Apr 03 2000 14:10:44   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0462                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0017                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                          *
      *    DATUM:    30.03.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00046!30.03.00!2.1 ! NEUERSTELLUNG             ! T. Moll  *
      * ASL00063!19.07.00!2.2 ! 36 Monate Karenz entfernen! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0462:                                *
      *       LESEN ALLER NEUEN VBA EINES ZEITRAUMS (OHNE DIREKTEIN- *
      *       STEIGER, NUR AUßENDIENST)  (FüR FöRDERBONUS)           *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0462.
          05 PI-DRVB0462-PROGRAMM           PIC  X(008).
          05 PI-DRVB0462-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0462-ERMITT-VON-ISO     PIC  X(010).
          05 PI-DRVB0462-ERMITT-BIS-ISO     PIC  X(010).
190700*   05 PI-DRVB0462-ANZ-MON-KARENZ     PIC  9(002).
      *
       01 PO-DRVB0462.

          05 PO-DRVB0462-RC                 PIC  X(003).
             88 PO-DRVB0462-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0462-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0462-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0462-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0462-DATEN.
             10 PO-DRVB0462-MA-ST-NR        PIC  S9(009) BINARY.
             10 PO-DRVB0462-VBA-SEIT-ISO    PIC   X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0462 *****************************
