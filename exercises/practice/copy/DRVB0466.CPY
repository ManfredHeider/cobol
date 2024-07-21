      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0466.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0466.CPv  $
      *
      *   Rev 2.2   02 Aug 2000 10:19:16   T13AE28
      *ASL00063: Entfernen 36-Monatsfrist
      *
      *   Rev 2.1   Apr 18 2000 12:43:02   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.0   Apr 03 2000 14:10:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0466                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0018                        *
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
      *    FÜR DIE FUNKTION DRVB0466:                                *
      *       LESEN ALLER NEUEN AL-P EINES ZEITRAUMS (OHNE DIREKTEIN-*
      *       STEIGER, NUR AUßENDIENST)  (FüR FöRDERBONUS)           *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0466.
          05 PI-DRVB0466-PROGRAMM           PIC  X(008).
          05 PI-DRVB0466-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0466-ERMITT-VON-ISO     PIC  X(010).
          05 PI-DRVB0466-ERMITT-BIS-ISO     PIC  X(010).
190700*   05 PI-DRVB0466-ANZ-MON-KARENZ     PIC  9(002).
      *
       01 PO-DRVB0466.

          05 PO-DRVB0466-RC                 PIC  X(003).
             88 PO-DRVB0466-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0466-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0466-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0466-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0466-DATEN.
             10 PO-DRVB0466-MA-ST-NR        PIC  S9(009) BINARY.
             10 PO-DRVB0466-ALP-SEIT-ISO    PIC   X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0466 *****************************
