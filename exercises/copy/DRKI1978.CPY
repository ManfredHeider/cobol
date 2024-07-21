      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1978.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1978.CPv  $
      *
      *   Rev 2.1   Dec 20 2006 13:55:38   t13ex43
      *FKI00979: Fremdverträge mit mehreren VN
      *
      *   Rev 2.0   Nov 03 2006 09:42:34   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1978                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWKI0359                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    18.12.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * FKI00979!18.12.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRKI1978 - ERMITTELN PERSONENID FÜR VERTRAG       *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRKI1978.
           05  PI-DRKI1978-DATEN.
               10  PI-DRKI1978-VERTRAGSID        PIC  X(013).
               10  PI-DRKI1978-PERSONENROLLEKZ   PIC  X(001).
      *
       01  PO-DRKI1978.
           05  PO-DRKI1978-RC                    PIC  X(003).
               88  PO-DRKI1978-RC-OK                  VALUE 'OK '.
               88  PO-DRKI1978-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRKI1978-DATEN.
               10  PO-DRKI1978-PERSONENID        PIC  X(013).
      ****
      **** ENDE COPY DRKI1978 ****************************************
      ****
