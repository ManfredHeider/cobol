      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2771.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2771.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:48:36   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:29:27 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2771                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2771                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.08.2013 / 09:37:51 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2771
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2771.
           05  PI-DRKI2771-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2771-BZBDBPGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2771-BZBDBPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2771-BZBDBPVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2771-BZBLFDNR   PIC S9(4) COMP.
END   *


           05  PI-DRKI2771-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2771-BZBABWBTGZAHLER   PIC  X(0001).
             10 PI-DRKI2771-BZBANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2771-BZBANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI2771-BZBBIC   PIC  X(0011).
             10 PI-DRKI2771-BZBGEBDATUM   PIC  X(010).
             10 PI-DRKI2771-BZBIBAN   PIC  X(0034).
             10 PI-DRKI2771-BZBMANDATREFNR.
                49 PI-DRKI2771-BZBMANDATREFNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2771-BZBMNDTRFNR-TEXT PIC X(35).
             10 PI-DRKI2771-BZBNACHNAME   PIC  X(0030).
             10 PI-DRKI2771-BZBVORNAME   PIC  X(0030).
END   *


      *
       01  PO-DRKI2771.
           05  PO-DRKI2771-RC                     PIC  X(003).
               88 PO-DRKI2771-RC-OK               VALUE 'OK '.
               88 PO-DRKI2771-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
