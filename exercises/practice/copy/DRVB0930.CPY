      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0930.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0930.CPv  $
      *
      *   Rev 2.1   Mar 24 2009 14:21:14   t13ae27
      *AAS00813: Einheiten neuer Partner
      *
      *   Rev 2.0   Mar 12 2009 14:18:10   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0930                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0930                    *
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
      *    FÜR DIE FUNKTION DRVB0930
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0930.
           05  PI-DRVB0930-PARTEH-BETR        PIC  S9(009)    COMP.
           05  PI-DRVB0930-PARTEH-UNTST       PIC  S9(009)    COMP.
           05  PI-DRVB0930-PARTEH-MONAT       PIC  X(010).
           05  PI-DRVB0930-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0930-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0930-PARTEH-URSPR-STRUK PIC  x(001).
      *
       01  PO-DRVB0930.
           05  PO-DRVB0930-RC                 PIC  X(003).
               88 PO-DRVB0930-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0930-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0930-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0930-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0930-PARTEH-TUPEL        PIC  X(094).
      *
      ******* ENDE COPY-MEMBER **************************************

