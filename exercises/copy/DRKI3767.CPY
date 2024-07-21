      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.12.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3767.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3767.CPv  $
      *
      *   Rev 2.1   Jan 09 2020 13:53:36   t13ex39
      *AKI01747: EU-DSGVO: Vertragslöschung der Deutschen Bank
      *
      *   Rev 2.0   Dec 12 2019 07:31:50 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3767                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3767                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.12.2019 / 07:31:50 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3767
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3767.
           05  PI-DRKI3767-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3767-GVDGESELLSCHAFT   PIC S9(4) COMP.
             10 PI-DRKI3767-GVDLAUFENDENUMMER   PIC S9(4) COMP.
             10 PI-DRKI3767-GVDVERTRAGSNUMMER   PIC  X(0015).
END   *

      *
       01  PO-DRKI3767.
           05  PO-DRKI3767-RC                     PIC  X(003).
               88 PO-DRKI3767-RC-OK               VALUE 'OK '.
               88 PO-DRKI3767-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3767-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3767-GVDDATUM   PIC  X(010).
             10 PO-DRKI3767-GVDVERTRAGSABLAUF   PIC  X(010).
             10 PO-DRKI3767-GVDVERTRAGSID   PIC  X(0013).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

