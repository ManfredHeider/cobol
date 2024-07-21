      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0725.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0725.CPv  $
      *
      *   Rev 2.1   Aug 12 2004 11:44:34   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung
      *
      *   Rev 2.0   Jul 09 2004 10:43:00   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0725                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0725                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    09.07.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00184!09.07.04! 01 ! NEU                       ! MUND     *
240322* VLA-4369!24.03.22! 02 ! Felddefinition COMP-3     ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0695
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0725.
           05  PI-DRVB0725-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0725-DATUM-VON          PIC  X(010).
           05  PI-DRVB0725-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0725.
           05  PO-DRVB0725-RC                 PIC  X(003).
               88 PO-DRVB0725-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0725-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0725-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0725-RC-ERR                       VALUE 'ERR'.
      *
240322     05  PO-DRVB0725-EH-FG-EG           PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

