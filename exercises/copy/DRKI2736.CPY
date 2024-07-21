      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2736.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2736.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:47:34   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 13:59:28   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:55:32 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2736                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2736                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.07.2013 / 15:34:39 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2736
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2736.
           05  PI-DRKI2736-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2736-BZSGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2736-BZSGHZFONDSID   PIC S9(4) COMP.
             10 PI-DRKI2736-BZSGHZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2736-BZSLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2736-BZSVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2736-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2736-BZSABWBTGZAHLER   PIC  X(0001).
             10 PI-DRKI2736-BZSANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2736-BZSANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI2736-BZSBIC   PIC  X(0011).
             10 PI-DRKI2736-BZSGEBDATUM   PIC  X(010).
             10 PI-DRKI2736-BZSIBAN   PIC  X(0034).
             10 PI-DRKI2736-BZSMANDATREFNR.
                49 PI-DRKI2736-BZSMANDATREFNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2736-BZSMNDTRFNR-TEXT PIC X(35).
             10 PI-DRKI2736-BZSNACHNAME   PIC  X(0030).
             10 PI-DRKI2736-BZSVORNAME   PIC  X(0030).
END   *

      *
       01  PO-DRKI2736.
           05  PO-DRKI2736-RC                     PIC  X(003).
               88 PO-DRKI2736-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2736-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
