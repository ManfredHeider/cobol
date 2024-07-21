      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0814.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0814.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 08:56:24   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Feb 01 2006 15:08:54   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0814                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0814                    *
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
      *    FÜR DIE FUNKTION DRVB0814
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0814.
           05  PI-DRVB0814-PARTEH-BETR        PIC  S9(009)    COMP.
           05  PI-DRVB0814-PARTEH-UNTST       PIC  S9(009)    COMP.
           05  PI-DRVB0814-PARTEH-MONAT       PIC  X(010).
           05  PI-DRVB0814-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0814-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0814-PARTEH-URSPR-STRUK PIC  x(001).
      *
       01  PO-DRVB0814.
           05  PO-DRVB0814-RC                 PIC  X(003).
               88 PO-DRVB0814-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0814-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0814-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0814-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0814-PARTEH-TUPEL        PIC  X(091).
      *
      ******* ENDE COPY-MEMBER **************************************

