      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1952.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1952.CPv  $
      *
      *   Rev 2.4   Sep 30 2009 14:50:50   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.3   Jan 23 2007 16:25:16   t13ex14
      *AKI00556: Ablösung PERSON_OKI_REF (UPKI)
      *
      *   Rev 2.2   Oct 30 2006 16:19:06   t13ex14
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Personen
      *
      *   Rev 2.1   Oct 06 2006 11:49:30   t13ex14
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Personen
      *
      *   Rev 2.0   Sep 12 2006 09:06:46   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1952                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1952                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  12.09.2006/09:43:28
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
310809* AKI00852!31.08.09! 02 ! SUCHNAME AUF 30 STELLEN   ! HRO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1952
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1952.
           05  PI-DRKI1952-KEY.


             10 PI-DRKI1952-PERSONENID   PIC X(13).
             
      *
       01  PO-DRKI1952.
           05  PO-DRKI1952-RC                 PIC  X(003).
               88 PO-DRKI1952-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1952-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1952-DATEN.


             10 PO-DRKI1952-PERSONANREDEID           PIC S9(4) COMP.
             10 PO-DRKI1952-NI-PERSONANREDEID        PIC S9(4) COMP.
             10 PO-DRKI1952-PERSONANZNAME            PIC X(30).
             10 PO-DRKI1952-NI-PERSONANZNAME         PIC S9(4) COMP.
310809*      10 PO-DRKI1952-PERSONANZVORNAME         PIC X(15).
310809       10 PO-DRKI1952-PERSONANZVORNAME         PIC X(30).
             10 PO-DRKI1952-NI-PERSONANZVORNAM       PIC S9(4) COMP.
310809*      10 PO-DRKI1952-PERSONEMPFOHLVON.
310809*         15 PO-DRKI1952-PRSNMPFHLVN-LN        PIC S9(4) COMP.
310809*         15 PO-DRKI1952-PRSNMPFHLVN-TXT       PIC X(50).
310809*      10 PO-DRKI1952-NI-PERSONEMPFOHLVN       PIC S9(4) COMP.
                
             10 PO-DRKI1952-PERSONGEBGRUENDDAT       PIC X(10).
             10 PO-DRKI1952-NI-PERSONGEBGRUEND       PIC S9(4) COMP.
310809*      10 PO-DRKI1952-PERSONKUNDENART          PIC X(01).
310809*      10 PO-DRKI1952-PERSONNAMEPHONCODE       PIC X(15).
310809*      10 PO-DRKI1952-NI-PERSONNAMEPHON        PIC S9(4) COMP.
310809*      10 PO-DRKI1952-PERSONNOTIZID            PIC X(13).
310809*      10 PO-DRKI1952-NI-PERSONNOTIZID         PIC S9(4) COMP.
310809*      10 PO-DRKI1952-PERSONSTEUERBERAT.
310809*         15 PO-DRKI1952-PRSNSTRBRT-LN         PIC S9(4) COMP.
310809*         15 PO-DRKI1952-PRSNSTRBRT-TXT        PIC X(50).
310809*      10 PO-DRKI1952-NI-PERSONSTEUERBER       PIC S9(4) COMP.
                
310809*      10 PO-DRKI1952-PERSONSTEUERBTELEF.
310809*         15 PO-DRKI1952-PRSNSTRBTLF-LN        PIC S9(4) COMP.
310809*         15 PO-DRKI1952-PRSNSTRBTLF-TXT       PIC X(20).
310809*      10 PO-DRKI1952-NI-PERSONSTEUERBTE       PIC S9(4) COMP.
                
310809*      10 PO-DRKI1952-PERSONSUCHNAME           PIC X(15).
310809       10 PO-DRKI1952-PERSONSUCHNAME           PIC X(30).
             10 PO-DRKI1952-NI-PERSONSUCHNAME        PIC S9(4) COMP.
310809*      10 PO-DRKI1952-PERSONSUCHVORNAME        PIC X(15).
310809       10 PO-DRKI1952-PERSONSUCHVORNAME        PIC X(30).
             10 PO-DRKI1952-NI-PERSONSUCHVORNA       PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
