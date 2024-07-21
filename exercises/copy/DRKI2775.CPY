      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2775.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2775.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:48:56   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:33:50 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2775                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2775                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.07.2013 / 16:33:50 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2775
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2775.
           05  PI-DRKI2775-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2775-BZKDBPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2775-BZKGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2775-BZKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2775-BZKVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2775-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2775-BZKABWBTGZAHLER   PIC  X(0001).
             10 PI-DRKI2775-BZKANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2775-BZKANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI2775-BZKBIC   PIC  X(0011).
             10 PI-DRKI2775-BZKGEBDATUM   PIC  X(010).
             10 PI-DRKI2775-BZKIBAN   PIC  X(0034).
             10 PI-DRKI2775-BZKMANDATREFNR.
                49 PI-DRKI2775-BZKMANDATREFNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2775-BZKMNDTRFNR-TEXT PIC X(35).
             10 PI-DRKI2775-BZKNACHNAME   PIC  X(0030).
             10 PI-DRKI2775-BZKVORNAME   PIC  X(0030).
END   *

      *
       01  PO-DRKI2775.
           05  PO-DRKI2775-RC                     PIC  X(003).
               88 PO-DRKI2775-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2775-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
