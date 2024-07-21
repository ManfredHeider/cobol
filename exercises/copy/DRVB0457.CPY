      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0457.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0457.CPv  $
      *
      *   Rev 2.1   Mar 29 2000 08:04:58   T13AE28
      *ASL00050: Lesen alle akt. OLBB-Sätze eines Betr.
      *
      *   Rev 2.0   Feb 07 2000 17:17:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0457                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0075                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.EHRMANN                                        *
      *    DATUM:    16.03.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00050!16.03.00! 01 ! NEU                       !D.EHRMANN *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0412                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0457.
          05 PI-DRVB0457-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0457-OLBB-BETR-NR       PIC S9(009)  COMP.
          05 PI-DRVB0457-OLBB-ERM-DAT       PIC  X(010).
      *
       01 PO-DRVB0457.
          05 PO-DRVB0457-RC                 PIC  X(003).
             88 PO-DRVB0457-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0457-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0457-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0457-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0457-DATEN.
             10 PO-DRVB0457-PARTNER-NR      PIC S9(009)  COMP.
             10 PO-DRVB0457-PARTNER-STUFE   PIC S9(004)  COMP.
             10 PO-DRVB0457-PARTNER-UMS     PIC S9(09)V9 COMP-3.

      *
      ***** ENDE COPY-MEMBER DRVB0412 *****************************

