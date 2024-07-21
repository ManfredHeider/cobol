      *AUTHOR.        M.SCHEIDEL.
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DSY03269!19.01.21! NEU                             ! SCHEIDEL
      *         !        !                                 !
      *         !        !                                 !
      *         !        !                                 !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *
      *   ABLEITUNG VERARBEITUNGSKENNZEICHEN
      *   - AUS DEN EINZELNEN VERARBEITUNGSKENNZEICHEN SOWIE EINEM
      *     JEWEILIGEN AENDERUNGSKENNZEICHEN WIRD DER GESAMTWERT DES
      *     VERARBEITUNGSKENNZEICHENS ERRECHNET.             
      *
      *****************************************************************
      *
        01 PI-DRPA1950.
           05 PI-DRPA1950-PGEB-GES          PIC S9(004) BINARY.
           05 PI-DRPA1950-VERARB-KZ         PIC  9(004).
           05 PI-DRPA1950-ALLE-KZ.
              10 PI-DRPA1950-KZ-VOR         PIC  X(001).
                 88 PO-DRPA1950-KZ-VOR-JA        VALUE 'J'.
                 88 PO-DRPA1950-KZ-VOR-NEIN      VALUE 'N'.
              10 PI-DRPA1950-KZ-VOR-AEND    PIC  X(001).
                 88 PO-DRPA1950-KZ-VOR-AEND-JA   VALUE 'J'.
              10 PI-DRPA1950-KZ-EAN         PIC  X(001).
                 88 PI-DRPA1950-KZ-EAN-JA        VALUE 'J'.
                 88 PI-DRPA1950-KZ-EAN-NEIN      VALUE 'N'.             
              10 PI-DRPA1950-KZ-EAN-AEND    PIC  X(001).
                 88 PO-DRPA1950-KZ-EAN-AEND-JA   VALUE 'J'.
              10 PI-DRPA1950-KZ-PNL         PIC  X(001).
                 88 PI-DRPA1950-KZ-PNL-JA        VALUE 'J'.
                 88 PI-DRPA1950-KZ-PNL-NEIN      VALUE 'N'.             
              10 PI-DRPA1950-KZ-PNL-AEND    PIC  X(001).
                 88 PO-DRPA1950-KZ-PNL-AEND-JA   VALUE 'J'.
              10 PI-DRPA1950-KZ-PDO         PIC  X(001).
                 88 PI-DRPA1950-KZ-PDO-JA        VALUE 'J'.
                 88 PI-DRPA1950-KZ-PDO-NEIN      VALUE 'N'.             
              10 PI-DRPA1950-KZ-PDO-AEND    PIC  X(001).
                 88 PO-DRPA1950-KZ-PDO-AEND-JA   VALUE 'J'.
              10 PI-DRPA1950-KZ-05          PIC  X(001).
                 88 PI-DRPA1950-KZ-05-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-05-NEIN       VALUE 'N'.             
              10 PI-DRPA1950-KZ-05-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-05-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-06          PIC  X(001).
                 88 PI-DRPA1950-KZ-06-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-06-NEIN       VALUE 'N'.             
              10 PI-DRPA1950-KZ-06-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-06-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-07          PIC  X(001).
                 88 PI-DRPA1950-KZ-07-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-07-NEIN       VALUE 'N'.             
              10 PI-DRPA1950-KZ-07-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-07-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-08          PIC  X(001).
                 88 PI-DRPA1950-KZ-08-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-08-NEIN       VALUE 'N'.             
              10 PI-DRPA1950-KZ-08-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-08-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-09          PIC  X(001).
                 88 PI-DRPA1950-KZ-09-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-09-NEIN       VALUE 'N'.             
              10 PI-DRPA1950-KZ-09-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-09-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-10          PIC  X(001).
                 88 PI-DRPA1950-KZ-10-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-10-NEIN       VALUE 'N'.             
              10 PI-DRPA1950-KZ-10-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-10-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-11          PIC  X(001).
                 88 PI-DRPA1950-KZ-11-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-11-NEIN       VALUE 'N'.             
              10 PI-DRPA1950-KZ-11-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-11-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-12          PIC  X(001).
                 88 PI-DRPA1950-KZ-12-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-12-NEIN       VALUE 'N'.
              10 PI-DRPA1950-KZ-12-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-12-AEND-JA    VALUE 'J'.
              10 PI-DRPA1950-KZ-13          PIC  X(001).
                 88 PI-DRPA1950-KZ-13-JA         VALUE 'J'.
                 88 PI-DRPA1950-KZ-13-NEIN       VALUE 'N'.
              10 PI-DRPA1950-KZ-13-AEND     PIC  X(001).
                 88 PO-DRPA1950-KZ-13-AEND-JA    VALUE 'J'.
           05 PI-DRPA1950-KZ-TAB 
              REDEFINES PI-DRPA1950-ALLE-KZ.
              10 FILLER                     OCCURS 13
                                            INDEXED BY IX-DRPA1950-KZ.  
                 15 PI-DRPA1950-KZ          PIC  X(001).
                    88 PI-DRPA1950-KZ-JA         VALUE 'J'.
                    88 PI-DRPA1950-KZ-NEIN       VALUE 'N'.      
                 15 PI-DRPA1950-KZ-AEND     PIC  X(001).
                    88 PI-DRPA1950-KZ-AEND-JA    VALUE 'J'.
        01 PO-DRPA1950.
           05 PO-DRPA1950-RC                PIC  X(003).
              88 PO-DRPA1950-RC-OK               VALUE 'OK '.
              88 PO-DRPA1950-RC-NOT-OK           VALUE 'NOK'.
              88 PO-DRPA1950-RC-GES-UNZUL        VALUE 'GUN'.
           05 PO-DRPA1950-DATEN.
              10 PO-DRPA1950-PGEB-GES       PIC S9(004) BINARY.
              10 PO-DRPA1950-VERARB-KZ      PIC  9(004).
