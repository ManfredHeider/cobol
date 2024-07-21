      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1186.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1186.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:28:26   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:53:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   13 Feb 2008 12:49:22   t13ex44
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Mar 16 1999 14:04:42   T01JM2S
      * 
      *
      *   Rev 2.0   Mar 05 1999 10:34:00   T01JM2S
      * 
      *
      *   Rev 1.0   21 Oct 1998 14:04:26   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1186                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN FÜR DRPA1181  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    08.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.03.99!2.1 ! NEU                       ! WOLFF    *
      * APA00622!13.02.08!2.2 ! KNR-VGES HINZU            ! SCHEIDEL *
      * APA01101!23.02.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1186                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1186.
          05 PI-DRPA1186-DATEN.
             10 PI-DRPA1186-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1186-PMBE-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1186.
          05 PO-DRPA1186-RC                         PIC  X(003).
             88 PO-DRPA1186-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1186-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1186-DATEN.
             10 PO-DRPA1186-KNR-PMBE-ART            PIC S9(004) BINARY.
             10 PO-DRPA1186-PVER-VERSION            PIC S9(004) BINARY.
             10 PO-DRPA1186-PMBE-RFOLGE-NR          PIC S9(004) BINARY.
             10 PO-DRPA1186-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1186-KNR-MANDANT             PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
