      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0755.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0755.CPv  $
      *
      *   Rev 2.1   Feb 22 2005 11:49:06   t13ex22
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005
      *. - Gesamt-EH bei ND berücksichtigt
      *
      *   Rev 2.0   Feb 21 2005 10:51:12   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0755                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0755                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    21.02.2005                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00513!21.02.05! 2.1! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0755
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0755.
           05  PI-DRVB0755-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0755-DATUM-VON          PIC  X(010).
           05  PI-DRVB0755-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0755.
           05  PO-DRVB0755-RC                 PIC  X(003).
               88 PO-DRVB0755-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0755-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0755-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0755-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0755-EH-GS-GR-EG         PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

