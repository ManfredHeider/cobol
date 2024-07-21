      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0775.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0775.CPv  $
      *
      *   Rev 2.1   May 23 2005 10:27:44   t13ae27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.0   Apr 15 2005 09:14:04   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0775                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0775                    *
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
      *    FÜR DIE FUNKTION DRVB0775
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0775.
           05  PI-DRVB0775-BETR               PIC  S9(009)    COMP.
           05  PI-DRVB0775-KNR-WB-TYP         PIC  S9(004)    COMP.
           05  PI-DRVB0775-KNR-WERTUNGS-TYP   PIC  S9(004)    COMP.
           05  PI-DRVB0775-DAT-VON            PIC  X(010).
           05  PI-DRVB0775-DAT-BIS            PIC  X(010).
           05  PI-DRVB0775-UMSATZ-KZ          PIC  S9(004)    COMP.
      *
       01  PO-DRVB0775.
           05  PO-DRVB0775-RC                 PIC  X(003).
               88 PO-DRVB0775-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0775-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0775-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0775-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0775-PARTEH-EH           PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

