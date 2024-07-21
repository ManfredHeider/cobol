      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0809.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0809.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 08:44:44   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Jan 26 2006 14:19:08   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0809                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0809                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    12.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00066!12.08.99!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0809
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0809.
           05  PI-DRVB0809-PARTEH-BETR        PIC  S9(009)    COMP.
           05  PI-DRVB0809-PARTEH-UNTST       PIC  S9(009)    COMP.
      *    05  PI-DRVB0809-PARTEH-MONAT       PIC  X(010).
           05  PI-DRVB0809-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0809-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0809-PARTEH-URSPR-STRUK PIC  X(001).
      *
       01  PO-DRVB0809.
           05  PO-DRVB0809-RC                 PIC  X(003).
               88 PO-DRVB0809-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0809-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0809-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0809-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0809-PARTEH-TUPEL        PIC  X(091).
      *
      ******* ENDE COPY-MEMBER **************************************

