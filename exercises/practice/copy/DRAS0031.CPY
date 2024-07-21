      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0031.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0031.CPv  $
      *
      *   Rev 2.1   Jul 22 2010 10:42:02   t13ae27
      *ASL00351: Columbusbonus - 300EEG-Regel - DETAILNP
      *
      *   Rev 2.0   Jul 08 2010 12:02:36   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0031                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0031                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    08.07.2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00251!08.07.10!    ! NEU                       ! Dürr     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAS0024
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0031.
           05  PI-DRAS0031-VB-NR              PIC  S9(009).
           05  PI-DRAS0031-KNR-WB-TYP         PIC  9(003).
           05  PI-DRAS0031-NP-TAB-D           PIC  X(265000).
           05  PI-DRAS0031-NP-TAB-I           PIC  X(265000).
           05  PI-DRAS0031-MONAT              PIC  X(010).
      *
       01  PO-DRAS0031.
           05  PO-DRAS0031-RC                 PIC  X(003).
               88 PO-DRAS0031-RC-OK                        VALUE 'OK '.
               88 PO-DRAS0031-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAS0031-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRAS0031-RC-ERR                       VALUE 'ERR'.
           05  PO-DRAS0031-ZAEHLER-D-XXX      PIC S9(009)       COMP-3.
           05  PO-DRAS0031-ZAEHLER-D-2XXX     PIC S9(009)       COMP-3.
           05  PO-DRAS0031-ZAEHLER-I-XXX      PIC S9(009)       COMP-3.
           05  PO-DRAS0031-ZAEHLER-I-2XXX     PIC S9(009)       COMP-3.





      *
      ******* ENDE COPY-MEMBER **************************************
