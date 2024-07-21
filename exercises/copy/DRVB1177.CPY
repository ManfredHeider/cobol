      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.08.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1177.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1177.CPv  $
      *
      *   Rev 2.1   Sep 18 2017 13:48:36   t13ae35
      *ASL00570: DVAG-Wien: Sonderleistungen ohne Finanzierungseinh
      *eiten [VB] - Lese-OZP ZWVB0058 (jhe)
      *
      *   Rev 2.0   Aug 17 2017 11:20:46   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1177                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1177                    *
      *    Gesamt-Geschäft ohne Finanzierung (USt.Frei)               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    17.08.2017                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00570!17.08.17! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1177
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1177.
           05  PI-DRVB1177-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1177-DATUM-VON          PIC  X(010).
           05  PI-DRVB1177-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1177.
           05  PO-DRVB1177-RC                 PIC  X(003).
               88 PO-DRVB1177-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1177-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1177-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1177-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1177-EH-GES-OF          PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 

