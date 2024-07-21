      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.01.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3768.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3768.CPv  $
      *
      *   Rev 2.1   Jan 27 2020 10:36:24   t13ac20
      *AKI01747: EU-DSGVO: Vertragslöschung der Deutschen Bank - An
      *passung DSGVO Source Db2
      *
      *   Rev 2.0   Jan 21 2020 07:57:26 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3768                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3768                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2020 / 07:57:26 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3768
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3768.
           05  PI-DRKI3768-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3768-GVDGESELLSCHAFT   PIC S9(4) COMP.
             10 PI-DRKI3768-GVDVERTRAGSABLAUF   PIC  X(010).
             10 PI-DRKI3768-GVDVERTRAGSNUMMER   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3768.
           05  PO-DRKI3768-RC                     PIC  X(003).
               88 PO-DRKI3768-RC-OK               VALUE 'OK '.
               88 PO-DRKI3768-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

