      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1748.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1748.CPv  $
      *
      *   Rev 2.5   Jan 06 2017 09:48:02   t13ex78
      *AKI01481: Änderungen UPKI0253
      *
      *   Rev 2.4   Dec 15 2016 12:13:02   t13ex78
      *AKI01481: OZPs und Funktionen
      *
      *   Rev 2.3   Oct 28 2015 19:31:24   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - An
      *passung ZTKI0077
      *
      *   Rev 2.2   Oct 28 2015 09:11:42   t13ex78
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - An
      *passung ZTKI0077
      *
      *   Rev 2.1   Feb 11 2005 14:06:16   t13ex39
      *FKI00909: AML-Abschlussvermittler-Breinigung
      *
      *   Rev 2.0   Jan 13 2003 15:52:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1748                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1748                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.01.2017 / 09:37:52 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI1748
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1748.
           05  PI-DRKI1748-KEY.
ANF   *        %M_PI_KEY%
             10 PI-DRKI1748-VERTRAGSID   PIC  X(0013).
END   *
      *
       01  PO-DRKI1748.
           05  PO-DRKI1748-RC                     PIC  X(003).
               88 PO-DRKI1748-RC-OK               VALUE 'OK '.
               88 PO-DRKI1748-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1748-DATEN.
ANF   *        %M_PO_DATEN%
             10 PO-DRKI1748-KDULAUFENDENUMMER   PIC S9(4) COMP.
END   *
      *
      ******* ENDE COPY-MEMBER **************************************

