      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0276.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0276.CPv  $
      *
      *   Rev 2.2   Feb 27 2012 13:23:50   t13ex15
      *AKI01121: Preisaktualisierung, Anpassung wegen neuen Attribu
      *tlängen
      *
      *   Rev 1.0   07 Jul 1998 11:49:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0276                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0276                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2012 / 11:40:30 / T13EX15
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
      *    FÜR DIE FUNKTION DRKI0276
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0276.
           05  PI-DRKI0276-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0276-RNPFONDID   PIC S9(4) COMP.
END   *


           05  PI-DRKI0276-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI0276-RNPAUSGABEBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI0276-RNPDATUMFONDSTAND   PIC  X(010).
             10 PI-DRKI0276-RNPDATUMWERTENTW   PIC  X(010).
             10 PI-DRKI0276-RNPRUECKNAHMEBETR   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI0276-RNPWERTENTWICKLUNG   PIC S9(5)V9(2) COMP-3.
END   *


      *
       01  PO-DRKI0276.
           05  PO-DRKI0276-RC                     PIC  X(003).
               88 PO-DRKI0276-RC-OK               VALUE 'OK '.
               88 PO-DRKI0276-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
