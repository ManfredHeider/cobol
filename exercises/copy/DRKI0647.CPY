      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0647.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0647.CPv  $
      *
      *   Rev 2.3   Feb 27 2012 13:24:00   t13ex15
      *AKI01121: Preisaktualisierung, Anpassung wegen neuen Attribu
      *tlängen
      *
      *   Rev 2.2   Jun 26 2002 13:37:12   T13EX39
      *AKI00274 PREISATTRIBUTE MIT NEUEN LÄNGEN
      *
      *   Rev 2.1   Oct 26 1999 16:27:48   T13EX39
      *SELECT mit aufgenommen
      *
      *   Rev 2.0   Feb 27 1999 12:58:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:26:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0647                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0647                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2012 / 11:40:32 / T13EX15
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0647
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0647.
           05  PI-DRKI0647-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0647-RNPFONDID   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI0647.
           05  PO-DRKI0647-RC                     PIC  X(003).
               88 PO-DRKI0647-RC-OK               VALUE 'OK '.
               88 PO-DRKI0647-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI0647-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI0647-RNPAUSGABEBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI0647-RNPDATUMFONDSTAND   PIC  X(010).
             10 PO-DRKI0647-RNPDATUMWERTENTW   PIC  X(010).
             10 PO-DRKI0647-RNPRUECKNAHMEBETR   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI0647-RNPWERTENTWICKLUNG   PIC S9(5)V9(2) COMP-3.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
