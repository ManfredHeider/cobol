      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3162.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3162.CPv  $
      *
      *   Rev 2.3   Aug 28 2017 09:58:44   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.2   Aug 23 2017 05:55:10   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.1   Aug 08 2017 14:25:10   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.0   Aug 4 2017 07:20:36 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3162                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3162                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.08.2017 / 07:20:36 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3162
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3162.
           05  PI-DRKI3162-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3162-BVHVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3162.
           05  PO-DRKI3162-RC                     PIC  X(003).
               88 PO-DRKI3162-RC-OK               VALUE 'OK '.
               88 PO-DRKI3162-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************



