      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0813.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0813.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 08:54:10   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Feb 01 2006 12:31:10   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0813                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0813                    *
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
      *    FÜR DIE FUNKTION DRVB0813
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0813.
           05  PI-DRVB0813-PARTEH-BETR        PIC  S9(009)    COMP.
           05  PI-DRVB0813-PARTEH-UNTST       PIC  S9(009)    COMP.
           05  PI-DRVB0813-PARTEH-MONAT       PIC  X(010).
           05  PI-DRVB0813-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0813-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0813-PARTEH-URSPR-STRUK PIC  x(001).
      *
       01  PO-DRVB0813.
           05  PO-DRVB0813-RC                 PIC  X(003).
               88 PO-DRVB0813-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0813-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0813-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0813-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0813-PARTEH-TUPEL        PIC  X(091).
      *
      ******* ENDE COPY-MEMBER **************************************

