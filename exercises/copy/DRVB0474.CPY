      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0474.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0474.CPv  $
      *
      *   Rev 2.1   10 May 2000 14:03:44   T13AE26
      *ASL00046 Förderbonus Auszahlung
      *
      *   Rev 2.0   Apr 03 2000 14:11:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0474                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0474                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    04.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00046!04.05.00!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0474: LESEN ST-NR
      *                               ERM. PRüFZIFFER UND Gesellschaft
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB0474.
           05  PI-DRVB0474-DATEN.
               10 PI-DRVB0474-ST-NR            PIC S9(009) COMP.
      *
       01  PO-DRVB0474.
           05  PO-DRVB0474-RC                  PIC  X(003).
               88 PO-DRVB0474-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0474-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0474-DATEN.
               10 PO-DRVB0474-ST-PRUEFZIFFER   PIC  X(001).
               10 PO-DRVB0474-KNR-GESELL-TYP   PIC S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
