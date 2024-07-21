      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0558.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0558.CPv  $
      *
      *   Rev 2.1   16 Oct 2001 11:53:00   t13ae30
      *FAS00072 - NEUE FUNKTION DRVB0558
      *
      *   Rev 2.0   15 May 2001 13:33:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0558                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0558                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    16.10.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAS00072!16.10.01!    ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0558
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01 PI-DRVB0558.
          05 PI-DRVB0558-PROGRAMM           PIC  X(008).
          05 PI-DRVB0558-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0558-ED-BESITZER        PIC  S9(009) COMP.
          05 PI-DRVB0558-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0558.
          05 PO-DRVB0558-RC                 PIC  X(003).
             88 PO-DRVB0558-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0558-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0558-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0558-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0558-DATEN.
             10 PO-DRVB0558-ED-KONTO         PIC  S9(009) COMP.
      *
      ******* ENDE COPY-MEMBER DRVB0558 ********************************
