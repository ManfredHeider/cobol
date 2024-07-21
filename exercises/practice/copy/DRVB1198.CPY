      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  31.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1198.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1198.CPv  $
      *
      *   Rev 2.3   Mar 26 2019 10:20:00   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gem‰ﬂ VB-Vertrag
      *
      *   Rev 2.2   Feb 20 2019 13:14:06   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gem‰ﬂ VB-Vertrag
      *
      *   Rev 2.1   Dec 11 2018 13:22:06   t13ex44
      *APA02659: komplette maschinelle Berechnung des Ausgleichsans
      *pruches gem‰ﬂ VB-Vertrag
      *
      *   Rev 2.0   Aug 31 2018 06:53:34   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1198 FUNKTION ZJVB0039                    *
      *    -------------------------------------------------------    *
      *    LESEN TAETIGKEITSDATUM                                     *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA02659!24.07.18!2.0 ! NEU                       ! SCHEIDEL  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHƒLT DIE EIN- UND AUSGABEPARAMETER          *
      *    F‹R DIE FUNKTION DRVB1198                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *****************************************************************
      *
       01 PI-DRVB1198.
          05 PI-DRVB1198-DATEN.
             10 PI-DRVB1198-VBNR       PIC S9(009) BINARY.
             10 PI-DRVB1198-KNR-GESELL-TYP
                                       PIC S9(004) BINARY.
      *
       01 PO-DRVB1198.
          05 PO-DRVB1198-RC            PIC  X(003).
             88 PO-DRVB1198-RC-OK           VALUE 'OK '.
             88 PO-DRVB1198-RC-NOT-FOUND    VALUE 'NFD'.
             88 PO-DRVB1198-RC-NOT-OK       VALUE 'NOK'.
          05 PO-DRVB1198-DATEN.
             10 PO-DRVB1198-MAT-DAT-VON
                                       PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

