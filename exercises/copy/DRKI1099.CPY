      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1099.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1099.CPv  $
      *
      *   Rev 2.3   Feb 27 2012 13:24:06   t13ex15
      *AKI01121: Preisaktualisierung, Anpassung wegen neuen Attribu
      *tlängen
      *
      *   Rev 2.2   Jun 26 2002 13:41:00   T13EX39
      *AKI00274 PREISATTRIBUTE MIT NEUEN LÄNGEN
      *
      *   Rev 2.1   21 Jul 2000 10:01:52   T13AE21
      *AKI00121
      *
      *   Rev 1.0   07 Jul 1998 11:51:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1099                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1099                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.02.2012 / 11:40:34 / T13EX15
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
      *    FÜR DIE FUNKTION DRKI1099
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1099.
           05  PI-DRKI1099-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1099-RNPFONDID   PIC S9(4) COMP.
END   *


           05  PI-DRKI1099-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI1099-RNPAUSGABEBETRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI1099-RNPDATUMFONDSTAND   PIC  X(010).
             10 PI-DRKI1099-RNPRUECKNAHMEBETR   PIC S9(7)V9(2) COMP-3.
END   *


      *
       01  PO-DRKI1099.
           05  PO-DRKI1099-RC                     PIC  X(003).
               88 PO-DRKI1099-RC-OK               VALUE 'OK '.
               88 PO-DRKI1099-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
